#
# RUN MODFLOW RES TEST PROBLEM OFR 96-364
#
Name=restest
#
TOPDIR=..
DATA=$TOPDIR/data
rm -f $Name.lst
ln -s $DATA/$Name.bcf fort.11
ln -s $DATA/$Name.ghb fort.17
ln -s $DATA/$Name.sip fort.19
ln -s $DATA/$Name.oc  fort.22
ln -s $DATA/$Name.res fort.27
$TOPDIR/bin/modflow <$DATA/$Name.bas >$Name.lst
rm -f fort.11 fort.17 fort.19 fort.22 fort.27
