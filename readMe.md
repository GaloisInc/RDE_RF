# Documentation Enricher (DER)

This is a tool to enrich documentation generated from the source code.
The tool parses the source code and adds additional information to it and generates a new documentation in LaTeX format.

## Supported Languages

* Lando
* SystemVerilog
* Bluespec SystemVerilog
* Cryptol
* SysML
* FRET (json format)

## Dependencies

The tool requires the following dependencies to be installed and in path:

* Cryptol
* Sbt (v 1.7)
* Latex
* Java (> v 8)
* Scala (>= v 2.13 && < v 3.0)
* BlueSpec Compiler
* Lobot
* Lando

Alternatively, you can use the docker image (simonthrane/document_enricher:latest) to run the tool.

## Usage

The tool is written in Scala. To run and build it, you need to have Java 8, Scala (2.13.6) and SBT installed on your machine.

## Usage Docker

To ease the usage of the tool, we provide a docker image. To run the tool, you need to have docker installed on your
machine.
To run the docker image, you need to pull the docker image and run it with the following command:

```bash
docker pull simonthrane/document_enricher:latest

docker run -v <path to the directory containing the documentation>:<srcFiles> simonthrane/document_enricher:latest -i <srcFiles> -o <srcFiles> <OptionalArguments>
```

Note that the docker image is large and might take a while to download.

Example of a command that compiles the latex document into an a4 pdf document and generate an overview of the
refinements in the project:

```bash
    docker run -v /home/user/Documents/Project/SourceFiles:/data simonthrane/document_enricher:latest -i /data -o /data -g -d=a4
```

Note that the docker image generates the documentation from the source code.
Therefore, you need to have the source code available in the local directory/volume that the docker image has access to.
This is the reason why we mount the local directory containing the source code into the docker image (using the -v option).
The docker image will then generate the documentation from the source code and put it into the same directory.

The following arguments are supported:

    * -i, --inputFolder <folder>    Required argument the directory returning the source code
    * -o, --outputFolder <folder>   Required argument the directory where the documentation should be generated
    * -e, --exclude <folder>        The directory to exclude from the documentation generation (e.g. the build directory or generic library files that should not be part of the generated documentation)
    * -f  --configFile <file>       The path to the explicit refinement configuration file.
    * -g, --generateLatex           Whether to generate the Pdf from the generated LaTeX documentation
        * -d, --dimension <value>        The dimension of the generated pdf document. Possible values are a4, b4. Default is a4.
        * -t, --title <value>            The title of the documentation
        * -a, --author <value>           The author of the documentation
    * -R, --GenerateRefinementOverview  Whether to generate the refinement overview
    * -V, --verifyAll         Whether to verify the source files by compilation all of them - it requires that the tools are installed and available in the path.
    * -v, --version           prints the version of the tool
    * -h, --help              prints this usage text

## Building the Docker Image

To build the docker image, you need to have docker and sbt installed on your machine.
Then, you can build the docker image by running the following command:

```bash
    sbt docker 
```

## Publishing the Docker Image

The docker image is published on docker hub. To publish the docker image, you need to have docker and sbt installed on your machine.
Then, you can publish the docker image by running the script:

```bash
    /bin/bash build_publish_docker.sh
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

Copyright (c) 2022-2023 Galois, Inc.
