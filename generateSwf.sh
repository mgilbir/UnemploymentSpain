#!/bin/sh

BASEDIR=./images/png
mkdir videos

for dir in `ls $BASEDIR`
do
	echo $dir
	png2swf -r 0.5 $BASEDIR/$dir/*.png -o ./videos/paro_trimestral_$dir.swf
	
done;