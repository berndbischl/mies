R	:= R --vanilla
RSCRIPT	:= Rscript --vanilla
DELETE	:= rm -fR

.SILENT:
.PHONEY: clean roxygenize package windows install test check

usage:
	echo "Available targets:"
	echo ""
	echo " clean         - Clean everything up"
	echo " roxygenize    - roxygenize skel/ into pkg_local"
	echo " package       - build source package"
	echo " windows       - build windows binary via win-builder/devtools"
	echo " install       - install the package"
	echo " test          - run RUnit tests"
	echo " check         - run R CMD check on the package"

clean:
	echo "Cleaning up ..."
	${DELETE} skel/src/*.o skel/src/*.so pkg_local.Rcheck
	${DELETE} pkg_local
	${DELETE} .RData .Rhistory

roxygenize: clean
	echo "Roxygenizing package ..."
	${RSCRIPT} ./tools/roxygenize
	#echo "Setting version ..."
	#${RSCRIPT} ./tools/set-version
 
package: roxygenize
	echo "Building package file ..."
	${R} CMD build pkg_local/

windows: roxygenize
	echo "Building windows binary ..."
	${RSCRIPT} ./tools/build-windows
  
install: roxygenize
	echo "Installing package ..."
	${R} CMD INSTALL pkg_local

test: install
	echo "Testing package ..."
	${RSCRIPT} ./test_all.R

check: roxygenize
	echo "Running R CMD check ..."
	${R} CMD check pkg_local

  
