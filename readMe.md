# Documentation Enricher (DER)

This is a tool to enrich documentation generated from the source code.
The tool parses the documentation and adds additional information to it and generates a new documentation in LaTeX format.

## Dependencies
The tool requires the following dependencies to be installed:

* Cryptol
* Sbt
* Latex
* Java

Alternatively, you can use the docker image (simonthrane/document_enricher:latest) to run the tool.

## Usage

The tool is written in Scala. To run it, you need to have Java 8 installed on your machine.

## Usage Docker

To run the tool, you need to pull the docker image and run it with the following command:

```bash
docker pull simonthrane/document_enricher:latest

docker run -v <path to the directory containing the documentation>:/data simonthrane/document_enricher:latest <Arguments>
```

The following arguments are supported:

    * -l, --generateLatex      Whether to generate the Pdf from the generated LaTeX documentation
    * -v, --verifyCryptolSpecifications  Whether to verify the Cryptol specifications
    *  -t --title <value>      The title of the documentation