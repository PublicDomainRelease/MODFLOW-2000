#
# RUN MODFLOW SAMPLE PROBLEM (TWRI 6-A1)
#
Name=twri
#
TOPDIR=..
DATA=$TOPDIR/data
rm -f $Name.lst
ln -s $DATA/$Name.bcf fort.11
ln -s $DATA/$Name.wel fort.12
ln -s $DATA/$Name.drn fort.13
ln -s $DATA/$Name.rch fort.18
ln -s $DATA/$Name.sip fort.19
$TOPDIR/bin/modflow <$DATA/$Name.bas >$Name.lst
rm -f fort.11 fort.12 fort.13 fort.18 fort.19
