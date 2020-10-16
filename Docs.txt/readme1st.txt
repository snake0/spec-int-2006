---------------------------------------------------
W A R N I N G      W A R N I N G      W A R N I N G
---------------------------------------------------
The following text file was automatically generated
from a document that you really should read in HTML
format.  This text document is only a poor fallback
if you cannot read HTML, but it is NOT RECOMMENDED.

To read this document in the recommended way, point
your favorite web browser at one of these 3 places:
(1) The SPEC site http://www.spec.org/cpu2006/Docs/
(2) The Docs directory from your CPU2006 DVD, e.g.:
    /dvdrom/Docs/ for Unix or E:\Docs\ for Windows.
(3) The Docs directory on the system where you have
    installed your SPEC CPU2006 tree - for example:
    /spec/Docs/ (Unix) or D:\myspec\Docs\ (Windows)
---------------------------------------------------
W A R N I N G      W A R N I N G      W A R N I N G
---------------------------------------------------

                          SPEC CPU2006: Read Me First

   Updated for SPEC CPU2006 (new features are highlighted)

   Last updated: 27 Jul 2006 jlh
   (To check for possible updates to this document, please see
   http://www.spec.org/cpu2006/Docs/.)

    Introduction

   This document provides background information about the SPEC CPU2006
   benchmark suite. SPEC hopes that this material will help you understand
   what the benchmark suite can, and cannot, provide; and that it will help
   you make efficient use of the product.

   Overall, SPEC designed SPEC CPU2006 to provide a comparative measure of
   compute intensive performance across the widest practical range of
   hardware. The product consists of source code benchmarks that are
   developed from real user applications. These benchmarks depend on the
   processor, memory and compiler on the tested system.

   This document is organized as a series of questions and answers.

   Background

   Q1. What is SPEC?

   Q2. What is a benchmark?

   Q3. Should I benchmark my own application?

   Q4. If not my own application, then what?

   Scope

   Q5. What does SPEC CPU2006 measure?

   Q6. Why use SPEC CPU2006?

   Q7. What are the limitations of SPEC CPU2006?

   Overview of usage

   Q8. What is included in the SPEC CPU2006 package?

   Q9. What does the user of the SPEC CPU2006 suite have to provide?

   Q10. What are the basic steps in running the benchmarks?

   Q11. What source code is provided? What exactly makes up these suites?

   Metrics

   Q12. Some of the benchmark names sound familiar; are these comparable to
   other programs?

   Q13. What metrics can be measured?

   Q14. What is the difference between a "base" metric and a "peak" metric?

   Q15. What is the difference between a "rate" and a "speed" metric?

   Q16. Which SPEC CPU2006 metric should be used to compare performance?

   CPU2006 vs. CPU2000

   Q17. SPEC CPU2000 is already available. Why create SPEC CPU2006? Will it
   show anything different?

   Q18. What happens to SPEC CPU2000 after SPEC CPU2006 is released?

   Q19. Is there a way to translate SPEC CPU2000 results to SPEC CPU2006
   results or vice versa?

   Benchmark selection

   Q20. What criteria were used to select the benchmarks?

   Q21. Weren't some of the SPEC CPU2006 benchmarks in SPEC CPU2000? How are
   they different?

   Q22. Why were some of the benchmarks not carried over from CPU2000?

   Miscellaneous

   Q23. Why does SPEC use a reference machine? What machine is used for SPEC
   CPU2006?

   Q24. How long does it take to run the SPEC CPU2006 benchmark suites?

   Q25. What if the tools cannot be run or built on a system? Can the
   benchmarks be run manually?

   Q26. Where are SPEC CPU2006 results available?

   Q27. Can SPEC CPU2006 results be published outside of the SPEC web site?
   Do the rules still apply?

   Q28. How do I contact SPEC for more information or for technical support?

   Q29. Now that I have read this document, what should I do next?

    Q1. What is SPEC?

   SPEC is the Standard Performance Evaluation Corporation. SPEC is a
   non-profit organization whose members include computer hardware vendors,
   software companies, universities, research organizations, systems
   integrators, publishers and consultants. SPEC's goal is to establish,
   maintain and endorse a standardized set of relevant benchmarks for
   computer systems. Although no one set of tests can fully characterize
   overall system performance, SPEC believes that the user community benefits
   from objective tests which can serve as a common reference point.

    Q2. What is a benchmark?

   A benchmark is "a standard of measurement or evaluation" (Webster's II
   Dictionary). A computer benchmark is typically a computer program that
   performs a strictly defined set of operations - a workload - and returns
   some form of result - a metric - describing how the tested computer
   performed. Computer benchmark metrics usually measure speed: how fast was
   the workload completed; or throughput: how many workload units per unit
   time were completed. Running the same computer benchmark on multiple
   computers allows a comparison to be made.

    Q3. Should I benchmark my own application?

   Ideally, the best comparison test for systems would be your own
   application with your own workload. Unfortunately, it is often impractical
   to get a wide base of reliable, repeatable and comparable measurements for
   different systems using your own application with your own workload.
   Problems might include generation of a good test case, confidentiality
   concerns, difficulty ensuring comparable conditions, time, money, or other
   constraints.

    Q4. If not my own application, then what?

   You may wish to consider using standardized benchmarks as a reference
   point. Ideally, a standardized benchmark will be portable, and may already
   have been run on the platforms that you are interested in. However, before
   you consider the results you need to be sure that you understand the
   correlation between your application/computing needs and what the
   benchmark is measuring. Are the benchmarks similar to the kinds of
   applications you run? Do the workloads have similar characteristics? Based
   on your answers to these questions, you can begin to see how the benchmark
   may approximate your reality.

   Note: A standardized benchmark can serve as reference point. Nevertheless,
   when you are doing vendor or product selection, SPEC does not claim that
   any standardized benchmark can replace benchmarking your own actual
   application.

    Q5. What does SPEC CPU2006 measure?

   SPEC CPU2006 focuses on compute intensive performance, which means these
   benchmarks emphasize the performance of:

     * the computer processor (CPU),
     * the memory architecture, and
     * the compilers.

   It is important to remember the contribution of the latter two components.
   SPEC CPU performance intentionally depends on more than just the
   processor.

   SPEC CPU2006 contains two components that focus on two different types of
   compute intensive performance:

     * The CINT2006 suite measures compute-intensive integer performance, and
     * The CFP2006 suite measures compute-intensive floating point
       performance.

   SPEC CPU2006 is not intended to stress other computer components such as
   networking, the operating system, graphics, or the I/O system. For
   single-CPU tests, the effects from such components on SPEC CPU2006
   performance are usually minor. For large rate runs, operating system
   services may affect performance, and the I/O system - number of disks,
   speed, striping - can have an effect. Note that there are many other SPEC
   benchmarks, including benchmarks that specifically focus on graphics,
   distributed Java computing, webservers, and network file systems.

    Q6. Why use SPEC CPU2006?

   SPEC CPU2006 provides a comparative measure of integer and/or floating
   point compute intensive performance. If this matches with the type of
   workloads you are interested in, SPEC CPU2006 provides a good reference
   point.

   Other advantages to using SPEC CPU2006 include:

     * The benchmark programs are developed from actual end-user
       applications, as opposed to being synthetic benchmarks.
     * Multiple vendors use the suite and support it.
     * SPEC CPU2006 is highly portable.
     * A wide range of results are available at http://www.spec.org
     * The benchmarks are required to be run and reported according to a set
       of rules to ensure comparability and repeatability.

    Q7. What are the limitations of SPEC CPU2006?

   As described above, the ideal benchmark for vendor or product selection
   would be your own workload on your own application. Please bear in mind
   that no standardized benchmark can provide a perfect model of the
   realities of your particular system and user community.

    Q8. What is included in the SPEC CPU2006 package?

   SPEC provides the following on the SPEC CPU2006 media (DVD):

     * Source code for the CINT2006 benchmarks
     * Source code for the CFP2006 benchmarks
     * A tool set for compiling, running, validating and reporting on the
       benchmarks
     * Pre-compiled tools for a variety of operating systems
     * Source code for the SPEC CPU2006 tools, for systems not covered by the
       pre-compiled tools
     * Run and reporting rules defining how the benchmarks should be used to
       produce SPEC CPU2006 results.
     * Documentation

    Q9. What does the user of the SPEC CPU2006 suite have to provide?

   Briefly, you need a Unix, Linux, Mac OS X, or Microsoft Windows system
   with compilers; 8GB of free disc space; and a minimum of 1GB of free
   memory - although more may be required, as described in
   system-requirements.html

   Note: links to SPEC CPU2006 documents on this web page assume that you are
   reading the page from a directory that also contains the other SPEC
   CPU2006 documents. If by some chance you are reading this web page from a
   location where the links do not work, try accessing the referenced
   documents at one of the following locations:

     * www.spec.org/cpu2006/Docs/
     * The $SPEC/Docs/ (Unix) or %SPEC%\Docs\ (Windows) directory on a system
       where SPEC CPU2006 has been installed.
     * The Docs/ directory on your SPEC CPU2006 distribution media.

    Q10. What are the basic steps in running the benchmarks?

   Installation and use are covered in detail in the SPEC CPU2006 User
   Documentation. The basic steps are:

     * Ensure that you meet the system requirements.
     * Install SPEC CPU2006 from the DVD on Unix, Linux, Mac OS X, or
       Microsoft Windows.
     * Determine which metric you wish to run.
     * Learn about runspec, which is the primary SPEC-provided tool.
     * Locate a configuration file as a starting point. Hints about where to
       find one are in runspec.html.
     * Use runspec to build (compile) the benchmarks.
     * If the above steps are successful, use runspec to run, validate, and
       create a report on the performance of the benchmarks.

   If you wish to generate results suitable for quoting in public, you will
   need to carefully study and adhere to the run rules.

    Q11. What source code is provided? What exactly makes up these suites?

   CINT2006 and CFP2006 are based on compute-intensive applications provided
   as source code. CINT2006 contains 12 benchmarks: 9 use C, and 3 use C++.
   The benchmarks are:

   400.perlbench  C   PERL Programming Language      
   401.bzip2      C   Compression                    
   403.gcc        C   C Compiler                     
   429.mcf        C   Combinatorial Optimization     
   445.gobmk      C   Artificial Intelligence: go    
   456.hmmer      C   Search Gene Sequence           
   458.sjeng      C   Artificial Intelligence: chess 
   462.libquantum C   Physics: Quantum Computing     
   464.h264ref    C   Video Compression              
   471.omnetpp    C++ Discrete Event Simulation      
   473.astar      C++ Path-finding Algorithms        
   483.xalancbmk  C++ XML Processing                 

   CFP2006 has 17 benchmarks: 4 use C++, 3 use C, 6 use Fortran, and 4 use a
   mixture of C and Fortran. The benchmarks are:

   410.bwaves    Fortran   Fluid Dynamics                   
   416.gamess    Fortran   Quantum Chemistry                
   433.milc      C         Physics: Quantum Chromodynamics  
   434.zeusmp    Fortran   Physics/CFD                      
   435.gromacs   C/Fortran Biochemistry/Molecular Dynamics  
   436.cactusADM C/Fortran Physics/General Relativity       
   437.leslie3d  Fortran   Fluid Dynamics                   
   444.namd      C++       Biology/Molecular Dynamics       
   447.dealII    C++       Finite Element Analysis          
   450.soplex    C++       Linear Programming, Optimization 
   453.povray    C++       Image Ray-tracing                
   454.calculix  C/Fortran Structural Mechanics             
   459.GemsFDTD  Fortran   Computational Electromagnetics   
   465.tonto     Fortran   Quantum Chemistry                
   470.lbm       C         Fluid Dynamics                   
   481.wrf       C/Fortran Weather Prediction               
   482.sphinx3   C         Speech recognition               

   Descriptions of the benchmarks, with reference to papers, web sites, and
   so forth, can be found in the individual benchmark descriptions (click the
   links above). Some of the benchmarks also provide additional details, such
   as documentation from the original program, in the nnn.benchmark/Docs
   directories in the SPEC benchmark tree.

   The numbers used as part of the benchmark names provide an identifier to
   help distinguish programs from one another. For example, some programs
   were updated from SPEC CPU2000 and need to be distinguished from the
   previous version. Note: even if a program has the same name as in a
   previous suite - for example, 176.gcc vs. 403.gcc - the updated workload
   and updated source code mean that it is not valid to compare SPEC CPU2006
   results to results with older SPEC CPU benchmarks.

    Q12. Some of the benchmark names sound familiar; are these comparable to
    other programs?

   Many of the SPEC benchmarks have been derived from publicly available
   application programs. The individual benchmarks in this suite may be
   similar, but are NOT identical to benchmarks or programs with similar
   names which may be available from sources other than SPEC. In particular,
   SPEC has invested significant effort to improve portability and to
   minimize hardware dependencies, to avoid unfairly favoring one hardware
   platform over another. For this reason, the application programs in this
   distribution may perform differently from commercially available versions
   of the same application.

   Therefore, it is not valid to compare SPEC CPU2006 benchmark results with
   anything other than other SPEC CPU2006 benchmark results.

    Q13. What metrics can be measured?

   After the benchmarks are run on the system under test (SUT), a ratio for
   each of them is calculated using the run time on the SUT and a
   SPEC-determined reference time. From these ratios, the following metrics
   are calculated:

   CINT2006 (for integer compute intensive performance comparisons):

     * SPECint2006: The geometric mean of twelve normalized ratios - one for
       each integer benchmark - when the benchmarks are compiled with peak
       tuning.
     * SPECint_base2006: The geometric mean of twelve normalized ratios when
       the benchmarks are compiled with base tuning.
     * SPECint_rate2006: The geometric mean of twelve normalized throughput
       ratios when the benchmarks are compiled with peak tuning.
     * SPECint_rate_base2006: The geometric mean of twelve normalized
       throughput ratios when the benchmarks are compiled with base tuning.

   CFP2006 (for floating point compute intensive performance comparisons:

     * SPECfp2006: The geometric mean of seventeen normalized ratios - one
       for each floating point benchmark - when compiled with peak tuning.
     * SPECfp_base2006: The geometric mean of seventeen normalized ratios
       when the benchmarks are compiled with base tuning.
     * SPECfp_rate2006: The geometric mean of seventeen normalized throughput
       ratios when the benchmarks are compiled with peak tuning.
     * SPECfp_rate_base2006: The geometric mean of seventeen normalized
       throughput ratios when the benchmarks are compiled with base tuning.

   In all cases, a higher score means "better performance" on the given
   workload.

    Q14. What is the difference between a "base" metric and a "peak" metric?

   In order to provide comparisons across different computer hardware, SPEC
   provides the benchmarks as source code. Thus, in order to run the
   benchmarks, they must be compiled. There is agreement that the benchmarks
   should be compiled the way users compile programs. But how do users
   compile programs?

     * Some users might experiment with many different compilers and compiler
       flags to achieve the best performance, and may be willing to develop
       multi-step make processes and "training" workloads.

     * Other users might prefer the relative simplicity of using a single set
       of switches and a single-step make process.

   In addition to the above, a wide range of other types of usage models
   could also be imagined, ranging in a continuum from -Odebug at the low
   end, to inserting directives and/or re-writing the source code at the high
   end. Which points on this continuum should SPEC CPU2006 allow?

   SPEC recognizes that any point chosen from that continuum might seem
   arbitrary to those whose interests lie at a different point. Nevertheless,
   choices must be made.

   For CPU2006, SPEC has chosen to allow two types of compilation:

     * The base metrics (e.g. SPECint_base2006) are required for all reported
       results and have stricter guidelines for compilation. For example, the
       same flags must be used in the same order for all benchmarks of a
       given language. This is the point closer to those who might prefer a
       relatively simple build process.

     * The peak metrics (e.g. SPECint2006) are optional and have less strict
       requirements. For example, different compiler options may be used on
       each benchmark, and feedback-directed optimization is allowed. This
       point is closer to those who may be willing to invest more time and
       effort in development of build procedures.

   Note that options allowed under the base metric rules are a subset of
   those allowed under the peak metric rules. A legal base result is also
   legal under the peak rules but a legal peak result is NOT necessarily
   legal under the base rules.

   A full description of the distinctions and required guidelines can be
   found in the SPEC CPU2006 Run and Reporting Rules.

    Q15. What is the difference between a "rate" and a "speed" metric?

   There are several different ways to measure computer performance. One way
   is to measure how fast the computer completes a single task; this is a
   speed measure. Another way is to measure how many tasks a computer can
   accomplish in a certain amount of time; this is called a throughput,
   capacity or rate measure.

     * The SPEC speed metrics (e.g., SPECint2006) are used for comparing the
       ability of a computer to complete single tasks.
     * The SPEC rate metrics (e.g., SPECint_rate2006) measure the throughput
       or rate of a machine carrying out a number of tasks.

   For the rate metrics, multiple copies of the benchmarks are run
   simultaneously. Typically, the number of copies is the same as the number
   of CPUs on the machine, but this is not a requirement. For example, it
   would be perfectly acceptable to run 63 copies of the benchmarks on a
   64-CPU machine (thereby leaving one CPU free to handle system overhead).

   Note: a speed run which uses a parallelizing compiler to distribute one
   copy of a benchmark over multiple CPUs is still a speed run, and uses the
   speed metrics. You can identify such runs by the field "Auto Parallel".

    Q16. Which SPEC CPU2006 metric should be used to compare performance?

   It depends on your needs. SPEC provides the benchmarks and results as
   tools for you to use. You need to determine how you use a computer or what
   your performance requirements are and then choose the appropriate SPEC
   benchmark or metrics.

   A single user running a compute-intensive integer program, for example,
   might only be interested in SPECint2006 or SPECint_base2006. On the other
   hand, a person who maintains a machine used by multiple scientists running
   floating point simulations might be more concerned with SPECfp_rate2006 or
   SPECfp_rate_base2006.

    Q17: SPEC CPU2000 is already available. Why create SPEC CPU2006? Will it
    show anything different?

   Technology is always improving. As the technology improves, the benchmarks
   should improve as well. SPEC needed to address the following issues:

   Run-time:
   As of summer, 2006, many of the CPU2000 benchmarks are finishing in less
   than a minute on leading-edge processors/systems. Small changes or
   fluctuations in system state or measurement conditions can therefore have
   significant impacts on the percentage of observed run time. SPEC chose to
   make run times for CPU2006 benchmarks longer to take into account future
   performance and prevent this from being an issue for the lifetime of the
   suites.

   Application size:
   As applications grow in complexity and size, CPU2000 becomes less
   representative of what runs on current systems. For CPU2006, SPEC included
   some programs with both larger resource requirements and more complex
   source code.

   Application type:
   SPEC felt that there were additional application areas that should be
   included in CPU2006 to increase variety and representation within the
   suites. For example, video compression and speech recognition have been
   added, and molecular biology has been significantly expanded.

   Moving target:
   CPU2000 has been available for six years and much improvement in hardware
   and software has occurred during this time. Benchmarks need to evolve to
   keep pace with improvements.

    Q18: What happens to SPEC CPU2000 after SPEC CPU2006 is released?

   SPEC will begin the process of retiring CPU2000. Three months after the
   announcement of CPU2006, SPEC will require all CPU2000 results submitted
   for publication on SPEC's web site to be accompanied by CPU2006 results.
   Six months after announcement, SPEC will stop accepting CPU2000 results
   for publication on its web site.

    Q19: Is there a way to translate SPEC CPU2000 results to SPEC CPU2006
    results or vice versa?

   There is no formula for converting CPU2000 results to CPU2006 results and
   vice versa; they are different products. There probably will be some
   correlation between CPU2000 and CPU2006 results (i.e., machines with
   higher CPU2000 results often will have higher CPU2006 results), but there
   is no universal formula for all systems.

   SPEC encourages SPEC licensees to publish CPU2006 numbers on older
   platforms to provide a historical perspective on performance.

    Q20: What criteria were used to select the benchmarks?

   In the process of selecting applications to use as benchmarks, SPEC
   considered the following criteria:

     * portability to a variety of CPU architectures (32- and 64-bit
       including AMD64, Intel IA32, Itanium, PA-RISC, PowerPC, SPARC, etc.)
     * portability to various operating systems, particularly UNIX and
       Windows
     * nearly all of the time is spent compute bound
     * little time spent in IO and system services
     * benchmarks should run in about 1GB RAM without swapping or paging
     * no more than five percent of benchmarking time should be spent
       processing code not provided by SPEC
     * well-known applications or application areas
     * available workloads that represent real problems

    Q21: Weren't some of the SPEC CPU2006 benchmarks in SPEC CPU2000? How are
    they different?

   Although some of the benchmarks from CPU2000 are included in CPU2006, they
   all have been given different workloads and/or modified to use newer
   versions of the source code. Therefore, for example, results with the
   CPU2000 benchmark 181.mcf may be strikingly different from results with
   the CPU2006 benchmark 429.mcf.

    Q22: Why were some of the benchmarks not carried over from CPU2000?

   Some benchmarks were not retained because it was not possible to create a
   longer-running or more robust workload. Others were left out because SPEC
   felt that they did not add significant performance information compared to
   the other benchmarks under consideration.

    Q23: Why does SPEC use a reference machine? What machine is used for SPEC
    CPU2006?

   SPEC uses a reference machine to normalize the performance metrics used in
   the CPU2006 suites. Each benchmark is run and measured on this machine to
   establish a reference time for that benchmark. These times are then used
   in the SPEC calculations.

   SPEC uses a historical Sun system, the "Ultra Enterprise 2" which was
   introduced in 1997, as the reference machine. The reference machine uses a
   296 MHz UltraSPARC II processor, as did the reference machine for CPU2000.
   But the reference machines for the two suites are not identical: the
   CPU2006 reference machine has substantially better caches, and the CPU2000
   reference machine could not have held enough memory to run CPU2006.

   It takes about 12 days to do a rule-conforming run of the base metrics for
   CINT2006 and CFP2006 on the CPU2006 reference machine.

   Note that when comparing any two two systems measured with the CPU2006,
   their performance relative to each other would remain the same even if a
   different reference machine was used. This is a consequence of the
   mathematics involved in calculating the individual and overall (geometric
   mean) metrics.

    Q24: How long does it take to run the SPEC CPU2006 benchmark suites?

   This depends on the suite and the machine that is running the benchmarks.
   As mentioned above, the reference (historical) machine takes on the order
   of 12 days; contemporary machines might take on the order of a couple
   days. Again, though, it depends on which metrics are run.

    Q25: What if the tools cannot be run or built on a system? Can the
    benchmarks be run manually?

   To generate rule-compliant results, an approved toolset must be used. If
   several attempts at using the SPEC-provided tools are not successful, you
   should contact SPEC for technical support. SPEC may be able to help you,
   but this is not always possible -- for example, if you are attempting to
   build the tools on a platform that is not available to SPEC.

   If you just want to work with the benchmarks and do not care to generate
   publishable results, SPEC provides information about how to do so.

    Q26: Where are SPEC CPU2006 results available?

   Results for measurements submitted to SPEC are available at
   http://www.spec.org/cpu2006.

    Q27: Can SPEC CPU2006 results be published outside of the SPEC web site? Do
    the rules still apply?

   Yes, SPEC CPU2006 results can be freely published if all the run and
   reporting rules have been followed. The CPU2006 license agreement binds
   every purchaser of the suite to the run and reporting rules if results are
   quoted in public. A full disclosure of the details of a performance
   measurement must be provided on request.

   SPEC strongly encourages that results be submitted for publication on
   SPEC's web site, since it ensures a peer review process and uniform
   presentation of all results.

   The run and reporting rules for research and and academic contexts
   recognize that it may not be practical to comply with the full set of
   rules in some contexts. It is always required, however, that non-compliant
   results must be clearly distinguished from rule-compliant results.

    Q28. How do I contact SPEC for more information or for technical support?

   SPEC can be contacted in several ways. For general information, including
   other means of contacting SPEC, please see SPEC's Web Site at:

   http://www.spec.org/

   General questions can be emailed to: info@spec.org
   CPU2006 Technical Support Questions can be sent to:
   cpu2006support@spec.org

    Q29. Now that I have read this document, what should I do next?

   If you haven't bought CPU2006, it is hoped that you will consider doing
   so. If you are ready to get started using the suite, then you should pick
   a system that meets the requirements as described in

   system-requirements.html

   and install the suite, following the instructions in

   install-guide-unix.html or
   install-guide-windows.html

   Questions and answers were prepared by Kaivalya Dixit of IBM, Jeff Reilly
   of Intel Corp, and John Henning of Sun Microsystems. Dixit was the
   long-time President of SPEC, Reilly is Chair of the SPEC CPU Subcommittee,
   and Henning is Vice-Chair/Secretary of the SPEC CPU Subcommittee.

     ----------------------------------------------------------------------

   Copyright (C) 1995-2006 Standard Performance Evaluation Corporation
   All Rights Reserved
