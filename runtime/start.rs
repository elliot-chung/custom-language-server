use std::{collections::{HashSet, HashMap}, env};

type SnekVal = u64;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(i64)]
pub enum ErrCode {
    InvalidArgument = 1,
    Overflow = 2,
    IndexOutOfBounds = 3,
    InvalidVecSize = 4,
    OutOfMemory = 5,
}

const TRUE: u64 = 7;
const FALSE: u64 = 3;

static mut HEAP_START: *const u64 = std::ptr::null();
static mut HEAP_END: *const u64 = std::ptr::null();

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64, heap_start: *const u64, heap_end: *const u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    if errcode == ErrCode::InvalidArgument as i64 {
        eprintln!("invalid argument");
    } else if errcode == ErrCode::Overflow as i64 {
        eprintln!("overflow");
    } else if errcode == ErrCode::IndexOutOfBounds as i64 {
        eprintln!("index out of bounds");
    } else if errcode == ErrCode::InvalidVecSize as i64 {
        eprintln!("vector size must be non-negative");
    } else {
        eprintln!("an error ocurred {}", errcode);
    }
    std::process::exit(errcode as i32);
}

#[export_name = "\x01snek_print"]
pub unsafe extern "C" fn snek_print(val: SnekVal) -> SnekVal {
    println!("{}", snek_str(val, &mut HashSet::new()));
    val
}

/// This function is called when the program needs to allocate `count` words of memory and there's no
/// space left. The function should try to clean up space by triggering a garbage collection. If there's
/// not enough space to hold `count` words after running the garbage collector, the program should terminate
/// with an `out of memory` error.
///
/// Args:
///     * `count`: The number of words the program is trying to allocate, including an extra word for
///       the size of the vector and an extra word to store metadata for the garbage collector, e.g.,
///       to allocate a vector of size 5, `count` will be 7.
///     * `heap_ptr`: The current position of the heap pointer (i.e., the value stored in `%r15`). It
///       is guaranteed that `heap_ptr + 8 * count > HEAP_END`, i.e., this function is only called if
///       there's not enough space to allocate `count` words.
///     * `stack_base`: A pointer to the "base" of the stack.
///     * `curr_rbp`: The value of `%rbp` in the stack frame that triggered the allocation.
///     * `curr_rsp`: The value of `%rsp` in the stack frame that triggered the allocation.
///
/// Returns:
///
/// The new heap pointer where the program should allocate the vector (i.e., the new value of `%r15`)
///
#[export_name = "\x01snek_try_gc"]
pub unsafe extern "C" fn snek_try_gc(
    count: isize,
    heap_ptr: *const u64,
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> u64 {
    let ptr = snek_gc(heap_ptr, stack_base, curr_rbp, curr_rsp);
    if ptr.add(count as usize) > unsafe { HEAP_END } {
        eprintln!("out of memory");
        std::process::exit(ErrCode::OutOfMemory as i32)
    } else {
        println!("GC");
        ptr as u64
    }  
}

/// This function should trigger garbage collection and return the updated heap pointer (i.e., the new
/// value of `%r15`). See [`snek_try_gc`] for a description of the meaning of the arguments.
#[export_name = "\x01snek_gc"]
pub unsafe extern "C" fn snek_gc(
    heap_ptr: *const u64,
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> *const u64 {
    let root_set = generate_root_set(curr_rsp, curr_rbp, stack_base);
    root_set.iter().for_each(|ptr| mark(*ptr));
    let new_heap_ptr = compute_forward_addr(heap_ptr);
    update_heap_references(heap_ptr);
    update_stack_references(curr_rsp, curr_rbp, stack_base);
    compact(heap_ptr);
    new_heap_ptr
}

fn compact(heap_ptr: *const u64) {
    let mut ptr = unsafe{ HEAP_START };
    while ptr < heap_ptr {
        let gc = unsafe{ *ptr };
        let size = unsafe{ *ptr.add(1) };
        if gc != 0 {
            let addr = gc as *mut u64;
            unsafe { ptr.copy_to(addr, (size + 2) as usize) };
            unsafe { *addr = 0 };
        }
        ptr = unsafe{ ptr.add((size + 2) as usize) };
    }
}

fn update_heap_references(heap_ptr: *const u64) {
    let mut ptr = unsafe{ HEAP_START as *mut u64}; 
    while (ptr as *const u64) < heap_ptr {
        let gc = unsafe{ *ptr };
        let size = unsafe{ *ptr.add(1) };
        if gc != 0 {
            for i in 0..size {
                let addr = unsafe{ ptr.add((i + 2) as usize) };
                let val = unsafe{ *addr };
                if val & 3 == 1 && val != 1 {
                    let old_addr = (val - 1) as *const u64;
                    let new_addr = unsafe{ *old_addr + 1 };
                    unsafe{ *addr = new_addr };
                }
            }
        }
        ptr = unsafe{ ptr.add((size + 2) as usize) };
    }
}

fn update_stack_references(curr_rsp: *const u64, curr_rbp: *const u64, stack_base: *const u64) {
    let mut addr_map: HashMap<u64, u64> = HashMap::new();
    let mut ptr = curr_rsp as *mut u64;
    let mut base_ptr = curr_rbp;
    while (ptr as *const u64) < stack_base {
        if (ptr as *const u64) == base_ptr {
            base_ptr = unsafe { *base_ptr as *const u64};
            ptr = unsafe{ ptr.add(2) };
        } else {
            let val = unsafe{ *ptr };
            if val & 3 == 1 && val != 1 {
                if addr_map.contains_key(&val) {
                    let new_addr = addr_map.get(&val).unwrap();
                    unsafe{ *ptr = *new_addr };
                } else {
                    let old_addr = (val - 1) as *const u64;
                    let new_addr = unsafe{ *old_addr + 1 };
                    unsafe{ *ptr = new_addr };
                    addr_map.insert(val, new_addr);
                }
            }
            ptr = unsafe{ ptr.add(1) };
        }
    }
}

fn compute_forward_addr(heap_ptr: *const u64) -> *const u64 {
    let mut ptr = unsafe{ HEAP_START as *mut u64 };
    let mut dst = ptr;
    while (ptr as *const u64) < heap_ptr {
        let marking = unsafe{ *ptr };
        let ptr_size = unsafe{ *ptr.add(1) };
        let next_ptr = unsafe{ ptr.add(2 + ptr_size as usize) };
        if marking == 1 {
            let next_dst = unsafe{ dst.add(2 + ptr_size as usize) };
            unsafe{ *ptr = dst as u64 };
            ptr = next_ptr;
            dst = next_dst;
        } else {
            ptr = next_ptr;
        }
    }
    dst
}

fn generate_root_set(curr_rsp: *const u64, curr_rbp: *const u64, stack_base: *const u64) -> HashSet<*mut u64> {
    let mut root_set: HashSet<*mut u64> = HashSet::new();
    let mut ptr = curr_rsp;
    let mut base_ptr = curr_rbp;
    while ptr < stack_base {
        if ptr == base_ptr {
            base_ptr = unsafe { *base_ptr as *const u64};
            ptr = unsafe{ ptr.add(2) };
        } else {
            let val = unsafe{ *ptr };
            if val & 3 == 1 && val != 1 {
                root_set.insert((val-1) as *mut u64);
            }
            ptr = unsafe{ ptr.add(1) };
        }
    }
    root_set
}

fn mark(obj_start: *const u64) {
    unsafe {*(obj_start as *mut u64) = 1};                       // Mark the GC word with a 1
    let size = unsafe{ *obj_start.add(1) };         // Get the size of the object
    let obj_end = unsafe{ obj_start.add(2 + size as usize) }; // Get the end of the object
    let mut ptr = unsafe{ obj_start.add(2) };       // Get the start of the object
    while (ptr as *const u64) < obj_end {                         // Terminate when we reach the end of the object
        let val = unsafe{ *ptr };          // Get the value of the word
        if val & 3 == 1 && val != 1 {                  // If the word is a pointer
            let ptr_val = (val-1) as *const u64; // Get the pointer value
            if unsafe{ *ptr_val } != 1 {               // If the value at the pointer is not marked
                mark(ptr_val);              // Mark the value at the pointer 
            }
        }
        ptr = unsafe{ ptr.add(1) };           // Move to the next word
    }
}

/// A helper function that can be called with the `(snek-printstack)` snek function. It prints the stack
/// See [`snek_try_gc`] for a description of the meaning of the arguments.
#[export_name = "\x01snek_print_stack"]
pub unsafe extern "C" fn snek_print_stack(
    stack_base: *const u64,
    _curr_rbp: *const u64,
    curr_rsp: *const u64,
) {
    let mut ptr = stack_base;
    println!("-----------------------------------------");
    while ptr >= curr_rsp {
        let val = *ptr;
        println!("{ptr:?}: {:#0x}", val);
        ptr = ptr.sub(1);
    }
    println!("-----------------------------------------");
}

#[export_name = "\x01snek_print_heap"]
pub unsafe extern "C" fn snek_print_heap(heap_ptr: *const u64) {
    let mut ptr = HEAP_START;
    println!("-----------------------------------------");
    while ptr < heap_ptr {
        let gc = *ptr;
        let size = *ptr.add(1);
        println!("{ptr:?}: {:#0x} {:#0x}", gc, size);
        for i in 0..size {
            let addr = ptr.add(2 + i as usize);
            let val = *addr;
            println!("    {addr:?}: {:#0x}", val);
        }
        ptr = ptr.add((size + 2) as usize);
    }
    println!("-----------------------------------------");
}

unsafe fn snek_str(val: SnekVal, seen: &mut HashSet<SnekVal>) -> String {
    if val == TRUE {
        format!("true")
    } else if val == FALSE {
        format!("false")
    } else if val & 1 == 0 {
        format!("{}", (val as i64) >> 1)
    } else if val == 1 {
        format!("nil")
    } else if val & 1 == 1 {
        if !seen.insert(val) {
            return "[...]".to_string();
        }
        let addr = (val - 1) as *const u64;
        let size = addr.add(1).read() as usize;
        let mut res = "[".to_string();
        for i in 0..size {
            let elem = addr.add(2 + i).read();
            res = res + &snek_str(elem, seen);
            if i < size - 1 {
                res = res + ", ";
            }
        }
        seen.remove(&val);
        res + "]"
    } else {
        format!("unknown value: {val}")
    }
}

fn parse_input(input: &str) -> u64 {
    match input {
        "true" => TRUE,
        "false" => FALSE,
        _ => (input.parse::<i64>().unwrap() << 1) as u64,
    }
}

fn parse_heap_size(input: &str) -> usize {
    input.parse::<usize>().unwrap()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() >= 2 { &args[1] } else { "false" };
    let heap_size = if args.len() >= 3 { &args[2] } else { "10000" };
    let input = parse_input(&input);
    let heap_size = parse_heap_size(&heap_size);

    // Initialize heap
    let mut heap: Vec<u64> = Vec::with_capacity(heap_size);
    unsafe {
        HEAP_START = heap.as_mut_ptr();
        HEAP_END = HEAP_START.add(heap_size);
    }

    let i: u64 = unsafe { our_code_starts_here(input, HEAP_START, HEAP_END) };
    unsafe { snek_print(i) };
}
