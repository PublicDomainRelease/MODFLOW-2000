#
Name=bcf2ss
#
TOPDIR=..
DATA=$TOPDIR/data
rm -f $Name.lst
ln -s $DATA/$Name.bcf fort.11
ln -s $DATA/$Name.wel fort.12
ln -s $DATA/$Name.riv fort.14
ln -s $DATA/$Name.rch fort.18
ln -s $DATA/$Name.pcg fort.19
ln -s $DATA/$Name.oc  fort.20
$TOPDIR/bin/modflow <$DATA/$Name.bas >$Name.lst
rm -f fort.11 fort.12 fort.14 fort.18 fort.19 fort.20
