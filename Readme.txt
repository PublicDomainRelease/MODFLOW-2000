README.TXT


                     MODFLOW-2000 - Version: 1.7 12/04/2001
         Three-dimensional finite-difference ground-water flow model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This version of MODFLOW-2000 is packaged for personal computers using
Microsoft Windows 95, 98, ME, NT, or 2000.

This version of MODFLOW is referred to as MODFLOW-2000 in order to
distinguish it from older versions.  See the file doc\Mf2k.txt for
descriptions, references, and additional contacts for this software.
Instructions for installation, execution, and testing are provided
below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. EXTRACTING FILES
                         C. COMPILING
                         D. INSTALLING
                         E. RUNNING THE SOFTWARE
                         F. TESTING


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

         mf2k1_7.exe

The distribution file contains:

          Compiled runfiles and source code for MODFLOW-2000.
          Compiled runfiles and source code for mf96to2k and mfpto2k
                    data conversion programs.
          Compiled runfiles and source code for the BEALE-2000,
                   YCINT-2000, RESAN-2000, HYDPOST, and HYDFMT
                   postprocessors.
          MODFLOW-2000 user guides in pdf files.
          Test data sets.


B. EXTRACTING FILES

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows you to specify the directory in which the files should
be restored.  The installation instructions assume that the files are
restored into directory C:\WRDAPP.  The following directory structure
will be created in C:\WRDAPP:


   |
   |--mf2k.1_7
   |    |--bin       ; compiled executables
   |    |--doc       ; documentation files
   |    |--test-win  ; batch files to run verification tests
   |    |--data      ; standard data sets used in verification tests
   |    |--src       ; source code
   |         |
   |         |--mf2k      ; MODFLOW-2000 source code
   |         |    |
   |         |    |serial    ; serial processing source code
   |         |    |parallel  ; parallel processing source code
   |         |
   |         |--beale2k   ; BEALE-2000 source code
   |         |--ycint2k   ; YCINT-2000 source code
   |         |--resan2k   ; RESAN-2000 source code
   |         |--mf96to2k  ; mf96to2k source code
   |         |--mfpto2k   ; mfpto2k source code
   |         |--hydprgm   ; HYDPOST and HYDFMT source code
   |


It is recommended that no user files are kept in the mf2k.1_7 directory
structure.  If you do plan to put files in the mf2k.1_7 directory
structure, do so only by creating subdirectories.

Included in directory mf2k.1_7\doc are Portable Document Format (PDF)
versions of the two MODFLOW-2000 user guides (OFR 00-92 and OFR 00-184)
and other related reports.

The PDF files are readable and printable on various computer platforms
using Acrobat Reader from Adobe.  The Acrobat Reader is freely
available from the following World Wide Web sites:
      http://www.adobe.com/
      http://www.shareware.com/

and by File Transfer Protocol (FTP) from the following site:
      ftp.adobe.com (path: /pub/adobe/acrobat)


C. COMPILING

Although executable versions of the programs are provided, the source
code is provided in the mf2k.1_7\src directory so that the programs can
be recompiled if necessary.  However, no support can be provided for
users generating their own versions of the software. In general, the
requirements are a Fortran compiler and the knowledge of using the
compiler.  When compiling MODFLOW-2000 for use on a serial computer,
the file mf2k.1_7\src\mf2k\serial\para-non.f must be compiled in
addition to the other MODFLOW-2000 source files.


D. INSTALLING

To make the executable versions of the programs accessible from any
directory, the directory containing the executables (mf2k.1_7\bin)
should be included in the PATH environment variable.  Also, if a
prior release of MODFLOW-2000 is installed on your system, the
directory containing the executables for the prior release should
be removed from the PATH environment variable.

As an alternative, all of the files in the mf2k.1_7\bin directory can
be copied into a directory already included in the PATH environment
variable.

       How to add to the PATH environment variable

On Windows9x and Windows ME systems, add the following line to the
AUTOEXEC.BAT file:

  PATH=%PATH%;C:\WRDAPP\mf2k.1_7\bin

Note, reboot your system after modifying AUTOEXEC.BAT.

On Windows NT systems, from the Start menu, select Settings and then
Control Panel.  Double-click System and select the Environment tab.
To add a new user variable, enter "PATH" in the Variable field and enter

   %PATH%;C:\WRDAPP\mf2k.1_7\bin

in the Value field.  Click Set and then click OK.  If a PATH user variable
already is defined, click on it in the User Variables pane, add
";C:\WRDAPP\mf2k.1_7\bin" to its definition in the Value field, and click
OK.  Initiate and use a new MS-DOS Command Prompt window after making this
change.

On Windows 2000 systems, from the Start menu, select Settings and then
Control Panel.  Double-click System and select the Advanced tab.  Click on
Environment Variables.  If a PATH user variable already is defined, click on
it in the User Variables pane, then click Edit.  In the Edit User Variable
window, add ";C:\WRDAPP\mf2k.1_7\bin" to the end of the Variable Value
(ensure that the current contents of the User Value are not deleted) and
click OK.  If a PATH user variable is not already defined, in the User
variables pane of the Environment Variables window, click New.  In the New
User Variable window, define a new variable PATH as shown above.  Click OK.
Click OK in the Environment Variables window and again in the System
Properties window.  Initiate and use a new MS-DOS Command Prompt window.


E. RUNNING THE SOFTWARE

MODFLOW-2000 and the postprocessors (RESAN-2000, BEALE-2000,
YCINT-2000, HYDPOST, and HYDFMT) have been compiled using the Lahey
Fortran 95 extended memory compiler version 5.60f.

The data arrays in MODFLOW-2000 are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, but this slows computations significantly.

After the files in the mf2k.1_7\bin directory are installed in a
directory that is included in your PATH, the programs are initiated in
a DOS Command-Prompt window using the commands:

          mf2k [Fname]
          mf96to2k
          mfpto2k
          beale2k [Fname]
          resan2k [Fname]
          ycint2k [Fname]
          hydpost
          hydfmt

The optional Fname argument to some of the programs is the name file. 
If no argument is used, the user is prompted to enter the name file. 
If the name file ends in ".nam", then the file name can be specified
without including ".nam".  For example, if the name file is named
abc.nam, then the simulation can be run by entering:

          mf2k abc

Starting with version 1.2 of MF2K, the mf2k runfile for use on personal
computers uses a different structure for unformatted files than has
been used in earlier versions distributed by the USGS.  Unformatted files
generally have a structure that is compiler specific.  Versions prior to
1.2 that were distributed by the USGS used a structure that was
specific to Lahey 77 and 90 Fortran.  This required that any program
that read unformatted files produced by these MODFLOW runfiles or any
program that generated unformatted files for use by MODFLOW had to be
compiled with one of these Lahey compilers.  For example, Zonebudget
and Modpath use unformatted budget files produced by MODFLOW.  Another
example is head files that are generated by one MODFLOW simulation and
used in a following simulation as initial heads.  Both simulations must
be run using a version of MODFLOW that uses the same unformatted file
structure.

The structure of unformatted files that is now used (since version 1.2)
in the MODFLOW runfile is one that is supported by a number of compiler
vendors (through the use of non-standard Fortran).  Therefore it will
be easier for others to use different compilers when compiling
applications that use or generate unformatted files.

This issue is described here so that users will be aware of the change
in format of the files read and written by the newest runfiles.  When a
version 1.2 or later runfile is used, then applications that read
unformatted MODFLOW files or produce unformatted files for use by
MODFLOW will have to be modified to use the options to read and write
the files with the new structure.  Also, unformatted head files that are
used as initial conditions in simulations must be created by a version
of the runfile that produces unformatted files in the new format.

The following support programs distributed by the USGS have been
recompiled to use the new unformatted file structure:  Zonebudget,
MODPATH, and MODPATH-PLOT.


F. TESTING

Test data sets are provided to verify that MODFLOW-2000 is correctly
installed and running on the system.  The tests may also be looked
at as examples of how to use the program.  The directory MF2K.1_7\data
contains the input data and expected results for each test.

The directory MF2K.1_7\test-win can be used to conveniently run the
tests without destroying the original results in the MF2K.1_7\data
directory.  MF2K.1_7\test-win contains batch (BAT) files to run the
tests.  Each test can be run by entering the name of the test as a
command in a DOS command-prompt window with the current directory being
MF2K.1_7\test-win or by double-clicking on the corresponding BAT file
in Windows Explorer.  The output files that are created in
MF2K.1_7\test-win can then be compared to those in MF2K.1_7\data.  The
tests are described in the table below.


test name      description of test
------------   -------------------------------------------------------
 TWRI          Example problem in OFR 00-92 without parameters
 TWRIP         Example problem in OFR 00-92 with parameters
 TC1OBSEN      Test Case 1 in OFR 00-184 with OBS and SEN, but not PES
 TC1           Test Case 1 in OFR 00-184 with PES and noise in
               observations
 TC1-TRUE      Test Case 1 in OFR 00-184 with PES and without
               observation noise 
 TC2           Test Case 2 in OFR 00-184
 TC3           Similar to Problem 4.2-2 in TWRI 3-B4 [Cooley and
               Naff (1990)]
 BCF2SS        Problem 1 in OFR 91-536 converted to MODFLOW-2000
 STR           Example problem in OFR 88-729 converted to MODFLOW-2000
 FHB           Example problem in OFR 97-571 converted to MODFLOW-2000
 RESTEST       Example problem in OFR 96-364 converted to MODFLOW-2000
 ETSDRT        Example described in OFR 00-466
 TC1HUF        Test case is described in OFR 00-342
 TC2HUFV4      Test case is described in OFR 00-342
 IBS2K         Storage-depletion test problem described in TWRI 6-A2
               converted to MODFLOW-2000
 L1A2K         Transient version of Test Simulation 1 in WRIR 00-4167
               converted to MODFLOW-2000
 L1B2K         Steady-state version of Test Simulation 1 in WRIR 00-4167
               converted to MODFLOW-2000
 TVP           Undocumented test case demonstrating the use of time-
               varying parameters.