The LLVM Compiler Infrastructure
================================

This directory and its subdirectories contain source code for LLVM,
a toolkit for the construction of highly optimized compilers,
optimizers, and runtime environments.

LLVM is open source software. You may freely distribute it under the terms of
the license agreement found in LICENSE.txt.

Please see the documentation provided in docs/ for further
assistance with LLVM, and in particular docs/GettingStarted.rst for getting
started with LLVM and docs/README.txt for an overview of LLVM's
documentation setup.

If you are writing a package for LLVM, see docs/Packaging.rst for our
suggestions.

About LLVM - JVM
=================
This repository contains code changes for a new JVM backend added to LLVM.

It generates java bytecode in format compliant with Jasmin Assembler.

Jasmin assembler integrated into backend can be invoked under command line option to generate .class file.

Backend currently generates subset of JVM bytecodes as per Java Card specification.

Granular change logs can be found @ doc/JVM_incrimental_commits_logs.txt
