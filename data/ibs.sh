#
Name=ibs
#
TOPDIR=..
DATA=$TOPDIR/data
rm -f $Name.lst
ln -s $DATA/$Name.bcf fort.7
ln -s $DATA/$Name.sip fort.9
ln -s $DATA/$Name.oc  fort.11
ln -s $DATA/$Name.ibs fort.13
$TOPDIR/bin/modflow <$DATA/$Name.bas >$Name.lst
rm -f fort.7 fort.11 fort.9 fort.13
