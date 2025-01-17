![GitHub Actions](https://img.shields.io/badge/github%20actions-%232671E5.svg?style=for-the-badge&logo=githubactions&logoColor=white)
[![Licence](https://img.shields.io/github/license/Ileriayo/markdown-badges?style=for-the-badge)](./LICENSE)

## The Prism Programming Language

Helloo, this is an imperative language implementation made during my years at EPITECH ! We named the language Prism and its a language inspired by Rust. It has a relatively simple and intuitive syntax as shown in the [BNF document](https://docs.google.com/document/d/1iVUNOgwsZKxAL-O0qcCsVGW6x6vrXbrLR_hJ9Pp6wmk/edit?usp=sharing). 

## Part 1

In its **first version**, Prism was equipped to handle various features, including signed integers in base 10 and support for symbols and lists. In order to test the early version of the language, you can compile the project using the `make` command. Once compiled, you can test the language using the provided example:

```bash
$> cat foo.scm
(define foo 21)
(* foo 2)
$> ./glados < foo.scm
42
```

It's worth noting that Prism's program execution halts as soon as an error is detected, and the interpreter exits with an error code of 84. This allows for efficient debugging and error handling during development. When dealing with boolean values, Prism uses the symbols `#t` to represent True and `#f` to represent False. These symbols provide a clear and concise way to work with logical operations within the language. To make the language usable and practical, we have implemented a set of built-in functions. The predicates `eq?` and `<` enable comparison operations, while the arithmetic operators '+', '-', '*', "div", and "mod" handle basic mathematical calculations. These core functions provide a solid foundation for building complex programs in Prism. Our goal were to explore the underlaying concepts behind the creation of a programming language.

## Part 2

In the second part, we needed to remove everything related to Symbolics Expressions and make our own language after generating our own AST (Abstract Syntax Tree).
We had the option to choose which paradigm(s) we want to implement and opted for the imperative one.
Since our language is still in the early stage, it has only few features and syntactic sugar. The project is separated into two binaries: the compiler binary (psc) and the VM binary (glados).

In order to improve our language, we will add some features such as:

- [x] Recursive calls
- [ ] A standard library
- [ ] Optimisation instructions
- [ ] String support, etc. 

We are aware that there is room for improvement and will try to do what's best for the language. We've provided some programs in the folder `testfiles/` in order for you to have a better understanding of our syntax.

The core features of the language have been tested using the Hspec framework. Details about the unit tests have been documented and are available [here](https://fun-5.gitbook.io/glados-dev-docs/test-policy).

## Building from Source
For this project, the compilation is done using a Makefile. Since the project is primarily distributed to Unix-like systems, the main requirements are **Make** and **Stack**

### Building on Unix-like systems
In order for you to build the project from Source on an Unix-like system, you need to clone the repository using `git clone <repository_url>` and run `make` at the root. The third-party libraries (hspec, binary, bytestring, containers, ...) will automatically be installed on the system.

## Getting Help
See [Developer docs](https://fun-5.gitbook.io/glados-dev-docs/) for technical details about our language.

## Troubleshooting
For any feature you want to add, please read our [Contributing.md](https://github.com/EpitechPromo2026/B-FUN-500-COT-5-2-glados-aman.menda/blob/36182ebc19af866d7de0ffcccbe8f69fcef7739d/CONTRIBUTING.md).

## License
Our language is primarily distributed under the terms of the MIT License.
See [MIT License](//fr.wikipedia.org/wiki/Licence_MIT) for details.
