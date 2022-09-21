# Documentation Enricher (DER)

This is a tool to enrich documentation generated from the source code.
The tool parses the source code and adds additional information to it and generates a new documentation in LaTeX format.

## Supported Languages

* Lando
* SystemVerilog
* Bluespec SystemVerilog
* Cryptol
* SysML

## Dependencies
The tool requires the following dependencies to be installed:

* Cryptol
* Sbt
* Latex
* Java
* Scala

Alternatively, you can use the docker image (simonthrane/document_enricher:latest) to run the tool.

## Usage

The tool is written in Scala. To run it, you need to have Java 8, Scala and SBT installed on your machine.

## Usage Docker

To ease the usage of the tool, we provide a docker image. To run the tool, you need to have docker installed on your machine.
To run the docker image, you need to pull the docker image and run it with the following command:

```bash
docker pull simonthrane/document_enricher:latest

docker run -v <path to the directory containing the documentation>:/data simonthrane/document_enricher:latest -s /data -t /data  <OptionalArguments>
```

Note that the docker image generates the documentation from the source code. 
Therefore, you need to have the source code available in the local directory/volume that the docker image has access to.


The following arguments are supported:

    * -s --source <value>     Required argument the directory returning the source code
    * -t --target <value>     Required argument the directory where the documentation should be generated
    * -l, --generateLatex      Whether to generate the Pdf from the generated LaTeX documentation
    * -v, --verifyCryptolSpecifications  Whether to verify the Cryptol specifications
    * -t --title <value>      The title of the documentation
    * -c, --refinementConfig <value>  The path to the explicit refinement configuration file.
    * -r, --Generate Refinement Overview  Whether to generate the refinement overview
    * -h, --help               prints this usage text

