all : main 

KYLIX=/usr/local/kylix
LIBPATH1=${KYLIX}/lib
LIBPATH2=${PWD}
LIB1=/lib
LIB2=/usr/lib
LIB3=${KYLIX}/bin
DCC=${KYLIX}/bin/dcc
EXEPATH=${PWD}
APP=lockbox
BINPATH=${KYLIX}/bin/

clean :
	find . -type f -iname "*~*" -exec rm {} \;
	find . -type f -iname "*.dcu" -exec rm {} \;
	find . -type f -iname "*.dpu" -exec rm {} \;
	find . -type f -iname "*.dcp" -exec rm {} \;
	find . -type f -iname "*.kof" -exec rm {} \;
	find . -type f -perm -755 -exec rm {} \;	

install :
	# move and symbolic link them
	cp bpltp${APP}${VERSION}r.so ${BINPATH}bpltp-${APP}${VERSION}-r.so.6.0
	cp bpltp${APP}${VERSION}d.so ${BINPATH}bpltp-${APP}${VERSION}-d.so.6.0
	ln -fs ${BINPATH}bpltp-${APP}${VERSION}-d.so.6.0 ${BINPATH}bpltp${APP}${VERSION}d.so
	ln -fs ${BINPATH}bpltp-${APP}${VERSION}-r.so.6.0 ${BINPATH}bpltp${APP}${VERSION}r.so 

rpminstall :
	# move them
	mv bpltp${APP}${VERSION}r.so ../packages/bpltp${APP}${VERSION}r.so
	mv bpltp${APP}${VERSION}d.so ../packages/bpltp${APP}${VERSION}d.so

main  : 
	# compile the libraries
	${DCC} -B -W -H -Q -O${LIB1} -O${LIB2} -O${LIB3} -U${LIBPATH1} -U${LIBPATH2} -E${EXEPATH} tp${APP}${VERSION}r.dpk
	${DCC} -B -W -H -Q -O${LIB1} -O${LIB2} -O${LIB3} -U${LIBPATH1} -U${LIBPATH2} -E${EXEPATH} tp${APP}${VERSION}d.dpk

