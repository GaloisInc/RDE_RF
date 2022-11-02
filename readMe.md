# Rigorous Digital Engineering (RDE) Refinement Finder

This is a tool track refinements through the RDE process.
The tool parses the source code written in some standard language 
often used in the RDE process (e.g., Lando, SysML, Cryptol, Bluespec SystemVerilog and SystemVerilog) 
and generates a graph of the refinement hierarchy.

The tool can furthermore enrich documentation generated from the source code.
The tool parses the source code and adds additional information to it and 
generates a new documentation in LaTeX format.

## Supported Languages

* Lando
* SystemVerilog
* Bluespec SystemVerilog
* Cryptol
* SysML

## Dependencies
The tool requires the following dependencies to be installed and in path:

* Cryptol
* Sbt
* Latex
* Java
* Scala
* BlueSpec Compiler
* Lobot
* Lando

Alternatively, you can use the docker image (simonthrane/document_enricher:latest) to run the tool.

## Usage

The tool is written in Scala. To run and build it, you need to have Java 8, Scala and SBT installed on your machine.

## Usage Docker

To ease the usage of the tool, we provide a docker image. To run the tool, you need to have docker installed on your machine.
To run the docker image, you need to pull the docker image and run it with the following command:

```bash
docker pull simonthrane/document_enricher:latest

docker run -v <path to the directory containing the documentation>:<srcFiles> simonthrane/document_enricher:latest -i <srcFiles> -o <srcFiles>  <OptionalArguments>
```

Note that the docker image is large and might take a while to download.

Example of a command that compiles the latex document into an a4 pdf document and generate an overview of the refinements in the project:

```bash

docker run -v /home/user/Documents/Project/SourceFiles:/data simonthrane/document_enricher:latest -i /data -o /data -g -d=a4
```

Note that the docker image generates the documentation from the source code. 
Therefore, you need to have the source code available in the local directory/volume that the docker image has access to.


The following arguments are supported:

    * -i, --inputFolder <folder>    Required argument the directory returning the source code
    * -o, --outputFolder <folder>   Required argument the directory where the documentation should be generated
    * -f  --configFile <file>       The path to the explicit refinement configuration file.
    * -g, --generateLatex           Whether to generate the Pdf from the generated LaTeX documentation
        * -d, --dimension <value>        The dimension of the generated pdf document. Possible values are a4, b4. Default is a4.
        * -t, --title <value>            The title of the documentation
    * -R, --GenerateRefinementOverview  Whether to generate the refinement overview
    * -A, --verifyAll         Whether to verify the source files by compilation all of them - it requires that the tools are installed and available in the path.  
    * -v, --version           prints the version of the tool
    * -h, --help              prints this usage text

## Building the Docker Image

To build the docker image, you need to have docker installed on your machine.
To build the docker image, you need to run the following command:

```bash
    sbt docker 
```

## Short-term Backlog 

* Add more elaborate support of languages (e.g. C, Lobot, Saw)
* Add support for verification of SAW specifications
* Improve parsing of Cryptol specifications
* Improve parsing of SystemVerilog specifications
* Your wish here!

## Long-term Backlog

* Add support for automatic refinement - of Lando into Cryptol.
* Add support for automatic refinement - of Cryptol into SystemVerilog.
* Include Yosys for automatic synthesis of SystemVerilog into Verilog.
* Include FRAMA-C for automatic verification of C code.
* Add support for AADL specifications.
* Add support for graphical representation of the refinement hierarchy (e.g. using GraphViz).

Copyright (c) 2022 Galois, Inc.
