README.TXT


                     MODFLOW-96 - Version: 3.3 2000/05/02
         Three-dimensional finite-difference ground-water flow model

The current version of MODFLOW is now referred to as MODFLOW-96 in order
to distinguish it from older versions.  For assistance, enhancement requests,
or bug reports contact the Hydrologic Analysis Software Support Program
by sending e-mail to h2osoft@usgs.gov.  See the file doc/modflw96.txt
for descriptions, references, and additional contacts for this software.
Instructions for installation, execution, and testing are provided below.


                            TABLE OF CONTENTS

                         A. FILES
                         B. EXTRACTING FILES
                         C. COMPILING
                         D. INSTALLING
                         E. RUNNING THE SOFTWARE
                         F. TESTING


A. FILES

The following distribution packages (containing the software, test data sets, 
and information files) are currently available for UNIX systems:

  modflw96.3_3.Solaris.tar.gz - Compiled for Sun UltraSPARC-II under Solaris 2.6
  modflw96.3_3.SGI.tar.gz     - Compiled for Silicon Graphics Indigo (32-bit)  
                                under Irix 6.5
  modflw96.3_3.source.tar.gz  - Source code

Included in directory modflw96.3_3/doc is a Portable Document Format (PDF)
version of the MODFLOW user's (ofr96485.pdf) and programmer's (ofr96586)
guides.  PostScript versions of these reports are available from:
      http://water.usgs.gov/software/modflow96.html

The PDF file is readable and printable on various computer platforms using
Acrobat Reader from Adobe.  The Acrobat Reader is freely available from
the following World Wide Web sites:
      http://www.adobe.com/
      http://www.shareware.com/

and by File Transfer Protocol (FTP) from the following site:
      ftp.adobe.com (path: /pub/adobe/acrobat)


B. EXTRACTING FILES

The compressed tar file, named modflw96.3_3.OS.tar.gz, contains all the files
needed to install and test MODFLOW-96 on a computer with a particular operating
system, where OS is a string indicating the operating system the distribution
is intended for.  If a version is not available for your operating system, the
file modflw96.3_3.source.tar.gz contains the source code and all other files
needed to compile, install, and test the software on a UNIX-based computer.
For either type of distribution, the directory modflw96.3_3 is created (or
overwritten) when the files are extracted from the tar file.  If the 
modflw96.3_3 directory already exists, you may want to delete or rename it 
before extracting the files.  The following are the steps to extract the files
from a distribution tar file.

   1. If the tar file is not already in the directory under which you want
      the distribution installed, move it there.  For example:
         mv modflw96.3_3.____.tar.gz /usr/opt/wrdapp

   2. If you are not in the directory where the tar file is located, go
      there.  For example:
         cd /usr/opt/wrdapp

   3. Uncompress the distribution file.  For example:
         gunzip modflw96.3_3.____.tar.gz

   4. Extract the distribution files from the tar file.  For example:
         tar -xpof modflw96.3_3.___.tar

      This creates the following directory structure (the contents of each
      directory are shown to the right):

      modflw96.3_3       ; copy of this README file
        `-----bin        ; compiled executable
        `-----doc        ; documentation files
        `-----src        ; Makefile and source code
        `-----test       ; scripts to run verification tests
        `-----data       ; standard data sets used in verification tests

Notes:
a) The bin directory is not included in the modflw96.3_3.source.tar.gz
   distribution (it is created during compilation).
b) Source code is included only with the modflw96.3_3.source.tar.gz
   distribution.
c) It is recommended that no user files be kept in the modflw96.3_3 directory
   structure.  If you do plan to put files in the modflw96.3_3 directory
   structure, do so only by creating subdirectories of modflw96.3_3.


C. COMPILING

If a compiled version of the software is not available for your computer, or
if you want to build the executable yourself, follow the instructions in this
section.  If you have retrieved a pre-compiled distribution of the software,
skip to the Installing section below.

The source code is provided in the modflw96.3_3.source.tar.gz distribution so
that users can generate the executable themselves.  No support can be
provided for users generating their own versions of the software.  In general,
the requirements are a Fortran compiler and a minimal level of knowledge of
the compiler and the UNIX operating system.  As provided, the Makefile and
source code are set up for use under Solaris.

To generate a new executable, do the following:

   1. Change directory to the source directory:
         cd modflw96.3_3/src

   2. Modify the beginning of the file named Makefile to correctly specify 
      system-dependent variables:
        
         F77          Fortran compiler name
         FFLAGS       Fortran compiler flags
    
   3. Use the make program to initiate compilation of the source code and
      installation of the software:
         make [BINDIR=directory_for_links]

      See the Installing instructions below for an explanation of BINDIR.

      The make will:
         a. create the directories modflw96.3_3/bin and BINDIR if they do not
            already exist,
         b. compile the source code,
         c. place the executable (modflow) in modflw96.3_3/bin, and
         d. place a link to the executable in BINDIR if specified.


D. INSTALLING

To make the executable (modflw96) easy to use, it should be installed in a
directory included in the user's search path.  The Makefile (input instructions
to the UNIX make program--located in modflw96.3_3/src) contains instructions to
optionally place a link in a specified directory to the executable contained in
modflw96.3_3/bin.  Use the following two commands to do this:

       cd modflw96.3_3/src
       make install [BINDIR=directory_for_links]

If BINDIR is specified, a link to the executable is placed in the specified
directory.  For example, if your search path consists of:
       /usr/bin:/usr/opt/bin:/usr/local/bin
use the command:
       make install BINDIR=/usr/local/bin
to make the executable accessible from any directory without requiring the full
pathname of the software's location.

Notes:
a) Brackets "[xxx]" are used to indicate optional arguments to commands.
b) To create and delete a link to the MODFLOW-96 executable file, the installer
   must have sufficient rights in the directory that BINDIR is set to.


E. RUNNING THE SOFTWARE

The X array is dimensioned to 1,500,000.  This is large enough for a model
having approximately 90,000 cells.

After MODFLOW is properly installed in a directory that is included in
your PATH, the program is initiated using the command: modflw96.  Unlike
earlier versions of MODFLOW, MODFLOW-96 uses a Name File to indicate
which files are used.  MODFLOW-96 prompts the user to enter the name of
the Name File, and then it automatically opens all the files specified
in the Name File.  Each record of the Name File specifies a file type,
unit number, and file name for each file used in the MODFLOW simulation.
In addition to defining the file names and unit numbers, the Name File
activates packages.  That is, the IUNIT elements that correspond to the
packages indicated by the file types are turned "on".  The IUNIT record
in the BAS Package input file is ignored; packages can now only be
activated through the Name File.  The Name File is read on unit 99, so
this unit should not be used for any other model files.  Refer to the
MODFLOW-96 documentation (USGS Open-File Report 96-485) for details
about preparing the Name File.


F. TESTING

Test data sets are provided to verify that the program is correctly installed
and running on the system.  The tests may also be looked at as examples of how
to use the program.  The directory "modflw96.3_3/test" contains the scripts to
run the tests.  The directory "modflw96.3_3/data" contains the input data and
expected results for each test.  Tests must be run in the directory
"modflw96.3_3/test".  Run the tests using any of the commands in the table
below.

To test the installation, change to the modflw96.3_3/test directory and type the
command:
      ./test.sh [m [n]]

where:
     m = the number of the first test to perform, default=1
     n = the number of the last test to perform, default=7

For example:

command                         what happens
------------------      ------------------------------------
./test.sh               runs all of the tests
./test.sh n             runs test 'n' through the last test
./test.sh n m           runs test 'n' through 'm'

After the tests are completed, the results are compared to the expected
results.  If all goes well, the only differences will be due to different
processing times or pathnames.  To clean up after the tests, type the
command:
      ./clean.sh

NOTE: the standard data sets included with the source code in
modflw96.3_3.source.tar.gz were created on a Sun UltraSPARC-II system.
You may notice slight numeric differences in the results on other computers.
These are generally due to different round-off algorithms and the different
architecture of the central processing unit chip.

The tests are described in the table below.  Test is the test number, program
is the program used to run the test, and the usage column indicates how a file
is used, with i for input, o for output, and i/o for both input and output.

test  program  description of test and files       file name & usage
----  -------  ---------------------------------   -----------------
  1   modflw96 Runs the storage depletion example in TWRI Book 6, Chapter A2

               Name File to designate files        ibs.nam        i
               BAS5 Package                        ibs.bas        i
               BCF5 Package                        ibs.bcf        i
               SIP5 Package                        ibs.sip        i
               Output Control                      ibs.oc         i
               IBS1 Package                        ibs.ibs        i
               Listing of results                  ibs.lst        o

  2   modflw96 Runs the example problem in OFR 88-729

               Name File to designate files        str.nam        i
               BAS5 Package                        str.bas        i
               BCF5 Package                        str.bcf        i
               SIP5 Package                        str.sip        i
               Output Control                      str.oc         i
               STR1 Package                        str.str        i
               Listing of results                  str.lst        o

  3   modflw96 Runs the problem in Appendix D of TWRI 6-A1

               Name File to designate files        twri.nam       i
               BAS5 Package                        twri.bas       i
               BCF5 Package                        twri.bcf       i
               WEL1 Package                        twri.wel       i
               DRN5 Package                        twri.drn       i
               RCH5 Package                        twri.rch       i
               SIP5 Package                        twri.sip       i
               Listing of results                  twri.lst       o

  4   modflw96 Runs problem 1 in OFR 91-536

               Name File to designate files        bcf2ss.nam     i
               BAS5 Package                        bcf2ss.bas     i
               BCF5 Package                        bcf2ss.bcf     i
               WEL1 Package                        bcf2ss.wel     i
               RIV5 Package                        bcf2ss.riv     i
               RCH5 Package                        bcf2ss.rch     i
               PCG2 Package                        bcf2ss.pcg     i
               Output Control                      bcf2ss.oc      i
               listing of results                  bcf2ss.lst     o

  5   modflw96 Runs the first problem in OFR 94-59 

               Name File to designate files        tlkp1.nam      i
               BAS5 Package                        tlkp1.bas      i
               BCF5 Package                        tlkp1.bcf      i
               TLK1 Package                        tlkp1.tlk      i
               SIP5 Package                        tlkp1.sip      i
               Output Control                      tlkp1.oc       i
               Listing of results                  tlkp1.lst      o
               cell-by-cell flows, storage         tlkp1.bud      o
               drawdown and heads results          tlkp1.ddn      o

  6   modflw96 Runs the example problem in OFR 96-364

               Name File to designate files        restest.nam    i
               BAS5 Package                        restest.bas    i
               BCF5 Package                        restest.bcf    i
               GHB5 Package                        restest.ghb    i
               SIP5 Package                        restest.sip    i
               Output Control                      restest.oc     i
               RES1 (Reservoir) Package            restest.res    i
               Listing of results                  restest.lst    o


  7   modflw96 Runs the example problem in OFR 97-571

               Name File to designate files        fhb.nam        i
               BAS5 Package                        fhb.bas        i
               BCF5 Package                        fhb.bcf        i
               SIP5 Package                        fhb.sip        i
               Output Control                      fhb.oc         i
               FBH1 (flow & head boundary) Package fhb.fhb        i
               Listing of results                  fhb.lst        o
               Binary file of head results         fhb.hd         o
               Binary file of cell-by-cell results fhb.cbc        o
