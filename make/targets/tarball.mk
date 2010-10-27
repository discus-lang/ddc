# Make a tarball for distribution

srcdir 		:= $(shell pwd)
datestamp 	:= $(shell date "+%Y%m%d")
dateseconds 	:= $(shell date "+%s")
tmpdir 		= ddc-head-$(dateseconds)
tarname 	= $(srcdir)/ddc-head-$(datestamp).tgz

.PHONY  : tarball
tarball :
	@echo "* Creating current tarball"
	@mkdir /tmp/$(tmpdir)
	@cd /tmp/$(tmpdir) && darcs get $(srcdir) ddc-head && tar zcf $(tarname) ddc-head
	@rm -rf /tmp/$(tmpdir)
	@chmod g+w,a+r $(tarname)
	@echo "* Tarball is :" $(tarname)
