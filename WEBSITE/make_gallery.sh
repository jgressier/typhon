#!/bin/sh

CASEDIR=../VALIDATION
SRCM4=$PWD/SRC
IMGDIR=$PWD/FILES/img/comput

for dir in $SRCM4 $CASESIR $IMGDIR ; do
  if [ ! -d $dir ] ; then
    echo !!! $0: $dir does not exist !!!
    exit 1
  fi
done

echo === making example website pages ===
cd $CASEDIR

m4prefix=example_
m4suffix=.html.m4
m4template=$SRCM4/case_template.m4
m4imgtemplate=$SRCM4/img_template.m4
m4maintemplate=$SRCM4/maincase_template.m4
m4maincase=$SRCM4/case$m4suffix
caselist=$SRCM4/source-case.make

tmpfile=/tmp/gallery.$$
tmpfile2=/tmp/gallery2.$$
tmpimg=/tmp/gallery-img.$$
trap "rm $tmpfile $tmpfile2 $tmpimg" SIGTERM SIGKILL SIGINT SIGHUP # man 7 signal

rm $SRCM4/key_*.list 2> /dev/null
echo CASEFIC=\\ > $caselist

for dir in $(find . -type d | grep -v \.svn) ; do
  descfile=$dir/webpage.desc
  if [ -f $descfile ] ; then
    #
    # HEADER OF CASE
    #
    echo \* parsing $dir

    desc_TITLE=$(   grep TITLE:    $descfile | sed "s/TITLE: *//" )
    desc_NAME=$(    grep NAME:     $descfile | sed "s/NAME: *//" )
    desc_SECTION=$( grep SECTION:  $descfile | sed "s/SECTION: *//" )
    desc_KEYWORDS=$(grep KEYWORDS: $descfile | sed "s/KEYWORDS: *//" )

    m4filename=$SRCM4/${m4prefix}${desc_NAME}${m4suffix}
    cp $m4template $m4filename

    cp $m4filename $tmpfile
    cat $tmpfile | sed -e "s/!!TITLE!!/${desc_TITLE}/" -e "s/!!NAME!!/${desc_NAME}/" -e "s/!!SECTION!!/${desc_SECTION}/" > $m4filename
    #
    # LOOP OVER IMAGES
    #
    for imgdescfile in $dir/*.imgdesc ; do
      imgdesc_TYPE=$(    grep TYPE:     $imgdescfile | sed "s/TYPE: *//" )
      imgdesc_IMGTITLE=$(grep IMGTITLE: $imgdescfile | sed "s/IMGTITLE: *//" )
      imgdesc_WIDTH=$(grep    WIDTH:    $imgdescfile | sed "s/WIDTH: *//" )
      imgfile=${imgdescfile%.imgdesc}.${imgdesc_TYPE}
      imgdesc_IMGFILE=${imgfile##*/}
 
      #cp $imgfile $IMGDIR/
      convert $imgfile -resize ${imgdesc_WIDTH}x $IMGDIR/$(basename $imgfile)
      convert $imgfile -resize x150              $IMGDIR/thumbnail-$(basename $imgfile)
      cp $m4imgtemplate $tmpimg
 
      cp $tmpimg $tmpfile
      cat $tmpfile | sed -e "s/!!IMGTITLE!!/${imgdesc_IMGTITLE}/" \
                         -e "s/!!IMGFILE!!/${imgdesc_IMGFILE}/"   \
                         -e "s/!!WIDTH!!/${imgdesc_WIDTH}/"  > $tmpimg

      grep IMGDESC: $imgdescfile | sed -e 's/IMGDESC: *//' -e 's/$/<BR>/' > $tmpfile

      # insert description in img file
      cat $tmpimg | sed "/!!IMGDESC!!/ { r $tmpfile
                         d }" > $tmpfile2
      cp $tmpfile2 $tmpimg

      # insert img file into case
      cp $m4filename $tmpfile
      cat $tmpfile | sed "/!!INSERT-IMG!!/ { r $tmpimg
                           N } ; " > $m4filename

    done
    cp $m4filename $tmpfile
    cat $tmpfile | sed "s/!!INSERT-IMG!!//" > $m4filename
    
    echo $(basename ${m4filename%.m4}) \\ >> $caselist

    # KEYWORDS management

    for key in $desc_KEYWORDS ; do
      keyfile=$SRCM4/key_$key.list
      echo 'item([hyperlink(['$desc_NAME'], ['${m4prefix}${desc_NAME}${m4suffix%.m4}']):' $desc_TITLE '])' >> $keyfile
    done
  fi
done 

# PROCESS KEYWORDS FILES

cp $m4maintemplate $m4maincase  

for keyfile in $SRCM4/key_*.list ; do
  keyword=${keyfile##*/key_}
  keyword=${keyword%%.list}
  echo '<span class="ghostitem" id="subitem_'$keyword'">'  > $tmpfile
  echo 'section(['$keyword examples'])'                   >> $tmpfile
  cat $keyfile                                            >> $tmpfile
  echo '<BR>'                                             >> $tmpfile
  echo '</span>'                                          >> $tmpfile
  cp $m4maincase $tmpfile2
  cat $tmpfile2 | sed "/!!INSERT-EXAMPLES!!/ { r $tmpfile
                           N } ; " > $m4maincase
  # add to THEME LIST
  echo 'item([m4_showitem(['$keyword'],['$keyword examples'])])' > $tmpfile
  cp $m4maincase $tmpfile2
  cat $tmpfile2 | sed "/!!THEME-LIST!!/ { r $tmpfile
                           N } ; " > $m4maincase

done  
cp $m4maincase $tmpfile2
cat $tmpfile2 | sed "s/!!INSERT-EXAMPLES!!//" > $m4maincase
cp $m4maincase $tmpfile2
cat $tmpfile2 | sed "s/!!THEME-LIST!!//" > $m4maincase
