C  COMMON definition for mf96to2k
      PARAMETER (IUCASE=1)
      PARAMETER (LENX=2000000)
      PARAMETER (MXFNAM=300,MXPER=250)
      COMMON X(LENX)
      DIMENSION IX(LENX)
      EQUIVALENCE (X,IX)
      COMMON /GLOBE/NDSNAM,ISUM,NLAY,NROW,NCOL,NFIL,INBCF,
     1     INBAS,IUNIT(40),IOUT,IUMISC,NMISS,NPER,ISS,
     2     IFREFM,ITMUNI,
     3     IUOPEN(MXFNAM),IUFILE(MXFNAM),IUMAP(4,2)
C
      COMMON /GLBCHR/DSNAME,HEADNG(2),
     1               CNF(MXFNAM),FN(MXFNAM),FORM(MXFNAM)
      CHARACTER      DSNAME*28,HEADNG*78,
     1               CNF*80,FN*80,FORM*12
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /BASCOM/NSTP(MXPER),PERLEN(MXPER),TSMULT(MXPER),
     1       IXSEC,ICHFLG,HNOFLO
      COMMON /BCFCOM/HDRY,IWETIT,WETFCT,IHDWET,IWDFLG,IBCFCB
      COMMON /XCOM/LCIBOU,LCHOLD,LCDELR,LCDELC,
     1        LCHY,LCTOP,LCBOT,LCCC,LCCV,LCSC1,LCSC2,LCTRPY,LCWETD
      COMMON /ARIBOU/CNIBOU(200),LOIBOU(200),FMIBOU(200),IPIBOU(200),
     1               MEIBOU(200),IBIBOU(200),FNIBOU(200)
      COMMON /ARHOLD/CNHOLD(200),LOHOLD(200),FMHOLD(200),IPHOLD(200),
     1               MEHOLD(200),IBHOLD(200),FNHOLD(200)
      COMMON /ARDELR/CNDELR,LODELR,FMDELR,IPDELR,
     1               MEDELR,FNDELR
      COMMON /ARDELC/CNDELC,LODELC,FMDELC,IPDELC,
     1               MEDELC,FNDELC
      COMMON /ARHY/CNHY(200),LOHY(200),FMHY(200),IPHY(200),
     1               MEHY(200),IBHY(200),FNHY(200)
      COMMON /ARTOP/CNTOP(200),LOTOP(200),FMTOP(200),IPTOP(200),
     1               METOP(200),IBTOP(200),FNTOP(200)
      COMMON /ARBOT/CNBOT(200),LOBOT(200),FMBOT(200),IPBOT(200),
     1               MEBOT(200),IBBOT(200),FNBOT(200)
      COMMON /ARCC/CNCC(200),LOCC(200),FMCC(200),IPCC(200),
     1               MECC(200),IBCC(200),FNCC(200)
      COMMON /ARCV/CNCV(200),LOCV(200),FMCV(200),IPCV(200),
     1               MECV(200),IBCV(200),FNCV(200)
      COMMON /ARSC1/CNSC1(200),LOSC1(200),FMSC1(200),IPSC1(200),
     1               MESC1(200),IBSC1(200),FNSC1(200)
      COMMON /ARSC2/CNSC2(200),LOSC2(200),FMSC2(200),IPSC2(200),
     1               MESC2(200),IBSC2(200),FNSC2(200)
      COMMON /ARTRPY/CNTRPY,LOTRPY,FMTRPY,IPTRPY,METRPY,FNTRPY
      COMMON /ARWETD/CNWETD(200),LOWETD(200),FMWETD(200),IPWETD(200),
     1               MEWETD(200),IBWETD(200),FNWETD(200)
      CHARACTER*20 FMIBOU,FMHOLD,FMDELR,FMDELC,FMHY,FMTOP,FMBOT,FMCC,
     1             FMCV,FMSC1,FMSC2,FMTRPY,FMWETD
      CHARACTER*80 FNIBOU,FNHOLD,FNDELR,FNDELC,FNHY,FNTOP,FNBOT,FNCC,
     1             FNCV,FNSC1,FNSC2,FNTRPY,FNWETD
      COMMON /FLWCOM/LAYCON(200)
      COMMON /FLWAVG/LAYAVG(200)
C
C Parameters that may require adjusting depending on the memory available
C on a specific computer:
C  IUCASE -- upper case flag -- 0=upper and lower case file names are
C         allowed; 1=uppercase file names only
C  LENX -- Length of the X array, which is used to store most model data.
C  MXFNAM -- Maximum number of file names
C  MXPER -- maximum number of stress periods.
C
C
C Definition of data in COMMON blocks:
C
C  ISUM -- Location in X array of next unused element
C  X -- array that holds most model data
C  NDSNAM -- Number of characters in the dataset name, DSNAME
C  DSNAME -- Dataset base name
C  IOUT -- real unit used for log file
C  IUMISC -- real unit that can be used for any I/O needed by a subroutine
C  MODOUT -- Flag for type of model output -- 0 for original MODFLOW format.
C         Not 0 for new free format output.
C  MODINP -- Flag for type of model input -- 0 for original fixed format.
C         Not 0 for free format.
C
C  NLAY -- Number of layers in model grid
C  NROW -- Number of roes in model grid
C  NCOL -- Number of columns in model grid
C  INBCF -- virtual unit used for BCF Package
C  INBAS -- virtual unit used for BAS package
C  IUNIT -- virtual units for packages -- elements have the same correspondence
C         to packages as for MODFLOW -- element 40 is for MODPATH
C  NPER -- Number of stress periods
C  ISS -- Steady-state flag -- 0 indicates transient
C  HEADNG -- Simulation title
C  NFIL -- Number of files in the name file
C
C  CNF(I) -- Actual text copy of line I in the Name file.
C  FILNAM(I) -- File name of virtual unit I.
C  FILTYP(I) -- File type of virtual unit I.
C  IUREAL(I) -- real unit number for virtual unit I.
C  IFNUSE(I) -- usage code for virtual unit I.
C          0 -- unused file (exception: IFNUSE is 0 for the listing file)
C         >0 -- virtual unit of the package file for the package that
C               makes use of this file
C         -1 -- unspecified use -- include in name file
C  MXUNIT -- maximum number of real file units that can be used (this
C         value is different for MODFLOW and MODPATH).
C  IUAVAI -- list of available real file units -- values must be between
C           1 and MXFNAM because these will be put into the name file and
C           can therefore become virtual files when the data are read as
C           old input by mfi.
C
C
