#
Name=str
#
TOPDIR=..
DATA=$TOPDIR/data
rm -f $Name.lst
ln -s $DATA/$Name.bcf fort.7
ln -s $DATA/$Name.sip fort.13
ln -s $DATA/$Name.oc  fort.14
ln -s $DATA/$Name.str fort.15
$TOPDIR/bin/modflow <$DATA/$Name.bas >$Name.lst
rm -f fort.7 fort.13 fort.14 fort.15
