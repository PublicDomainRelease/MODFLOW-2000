#
Name=tlkp1
#
TOPDIR=..
DATA=$TOPDIR/data
rm -f $Name.lst $Name.drw $Name.flw
ln -s $Name.drw fort.61
ln -s $Name.flw fort.63
ln -s $DATA/$Name.bcf fort.7
ln -s $DATA/$Name.tlk fort.9
ln -s $DATA/$Name.sip fort.12
ln -s $DATA/$Name.oc  fort.60
$TOPDIR/bin/modflow <$DATA/$Name.bas >$Name.lst
rm fort.7 fort.9 fort.12 fort.60 fort.61 fort.63
