C     Last change:  ERB  28 Mar 2001   11:20 am
C  Data definitions for Named Parameters
      PARAMETER (MXPAR=500,MXCLST=1000,MXZON=200,MXMLT=200)
      COMMON /GLOCOM/B(MXPAR),IPSUM,ICLSUM,IDEFPAR,
     1               IPLOC(2,MXPAR),IACTIVE(MXPAR),IPCLST(14,MXCLST),
     2               IPPTR(MXPAR)
      COMMON /GLOCOMC/ZONNAM(MXZON),MLTNAM(MXMLT),PARNAM(MXPAR),
     1               PARTYP(MXPAR)
      CHARACTER*10 ZONNAM,MLTNAM,PARNAM
      CHARACTER*4  PARTYP
C
C  IPSUM -- Number of parameters that have been defined
C  ICLSUM -- Number of clusters that have been defined
C  IDEFPAR -- Flag that is changed from 0 to 1 when parameter definition
C             is complete
C
C  "p" indicates a parameter number
C  IPLOC(n,p)     n=1 -- first cluster or list location
C                 n=2 -- last cluster or list location
C  IACTIVE(p) -- flag indicating if parameter is active in the current time step
C                            
C  B(p) -- Parameter value
C  PARNAM(p) -- Parameter name
C  PARTYP(p) -- Parameter type
C
C  "c" indicates a cluster
C  IPCLST(n,c) --      n=1 layer #
C                      n=2 multiplier array number (0 indicates none)
C                      n=3 zone array number (0 indicates all cells)
C                      n=4 index of last zone number for this cluster
C                      n=5-14 zone numbers
C
C  IPPTR(p) -- Pointer to parameter number for parameters which are to 
C              undergo estimation or sensitivity analysis, that is, 
C              parameters for which ISENS is greater than zero.
C
C  "z" indicates the zone array number
C  IZON(NCOL,NROW,z) -- Zone array
C  ZONNAM(z) -- Zone array name
C
C  "m" indicates the multiplier array number
C  RMLT(NCOL,NROW,m) -- Multiplier array
C  MLTNAM(m) -- multiplier array name
