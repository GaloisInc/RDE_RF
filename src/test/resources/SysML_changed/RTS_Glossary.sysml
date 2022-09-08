/*
 # Reactor Trip System (RTS) High-assurance Demonstrator
 ## project: High Assurance Rigorous Digital Engineering for Nuclear Safety (HARDENS)
 ### copyright (C) 2021-2022 Galois
 ### author: Joe Kiniry <kiniry@galois.com>
*/

// @see https://github.com/GaloisInc/HARDENS/issues/30

package id Glossary 'Project Glossary' {
  // Original proposal glossary.
  part def BlueCheck;

  abstract item id BISL 'Behavioral Interface Specification Language';
  abstract part def Coq;
  abstract part def Cryptol;
  abstract item def DevSecOps;
  abstract item def id DIANC 'Digital Instrumentation and Control Systems';
  /** The NASA Formal Requirements Elicitation Tool is used to make writing,
      understanding, and debugging formal requirements natural and
      intuitive. */
  part def id FRET 'Formal Requirements Elicitation Tool';
  /** An Instruction Set Architecture, or ISA for short, is the set of
      instructions that a given kind of CPU can understand. Example ISAs
      include x86, x64, MIPS, RISC, RISC-V, AVR, etc. */
  attribute def id ISA 'Instruction Set Architecture';
  /** A specification language integrated with support tools and an
      automated theorem prover, developed at the Computer Science Laboratory
      of SRI International.  PVS is based on a kernel consisting of an
      extension of Church's theory of types with dependent types, and is
      fundamentally a classical typed higher-order logic. */
  part def PVS;
  /** RISC-V (pronounced ``risk-five'') is an open standard instruction set
      architecture (ISA) based on established reduced instruction set
      computer (RISC) principles. Unlike most other ISA designs, the RISC-V
      ISA is provided under open source licenses that do not require fees to
      use. A number of companies are offering or have announced RISC-V
      hardware, open source operating systems with RISC-V support are
      available and the instruction set is supported in several popular
      software toolchains. */
  attribute def RISC_V_ISA :> ISA;
  /** A formal specification language that uses hierarchical finite state
      machines to specify system requirements. */
  part def id RSML 'Requirements State Modeling Language';
  /** The Boolean satisfiability problem (sometimes called propositional
      satisfiability problem and abbreviated SAT) is the problem of
      determining if there exists an interpretation that satisfies a given
      Boolean formula. */
  abstract item def SAT;
  /** The proof script language is used to specify the assumptions and proof
      goals of formal verifications to the SAW tool. */
  part def SAWscript;
  /** A CPU or SoC that is implemented in an HDL and synthesized to a
      bitstream and loaded onto an FPGA. */
  /** A formally defined computer programming language based on the Ada
      programming language, intended for the development of high integrity
      software used in systems where predictable and highly reliable
      operation is essential. It facilitates the development of applications
      that demand safety, security, or business integrity. */
  part def SPARK;
  /** An integrated development environment for formally specifying and
      rigorously analyzing requirements. */
  part def SpeAR;
  /** VCC is a program verification tool that proves correctness of
      annotated concurrent C programs or finds problems in them. VCC extends
      C with design by contract features, like pre- and postcondition as
      well as type invariants. Annotated programs are translated to logical
      formulas using the Boogie tool, which passes them to an automated SMT
      solver Z3 to check their validity. */
  part def id VCC 'Verifier for Concurrent C';
  /** A software toolchain that includes static analyzers to check
      assertions about a C program; optimizing compilers to translate a C
      program to machine language; and operating systems and libraries to
      supply context for the C program. The Verified Software Toolchain
      project assures with machine-checked proofs that the assertions
      claimed at the top of the toolchain really hold in the
      machine-language program, running in the operating-system context. */
  part def id VST 'Verified Software Toolchain';

}
