# Compiler Server - [Link to Interactive Client](https://github.com/ec-2018/custom-language-client)

## Overview

The is the cloud compiler for a custom programming language I wrote for a university assignment for a course on compilers. The original project was to develop a programming language and accompanying compiler over the course of the quarter. This version includes a webserver written in Go which opens a network endpoint for compiling the language online. The server is deployed using [fly.io](fly.io).

## Compiler

A Rust program is used to parse instructions into x86 instructions which are then compiled by the NASM assembler. Another rust program is used as a runtime to stage heap memory and link to the binary produced by the assembler. This entire process is automated by a Makefile so an instruction file can produce an executable in a single CLI command.

## Server

Each connection made to the web server is supposed to be accompanied by instruction text. The server creates a temporary workspace for each connection which is used to produce and run an executable. The result (a compilation error, a runtime error, or a successful output) is sent back as a response. If either compilation or runtime takes longer than 30 seconds, the process times out.
