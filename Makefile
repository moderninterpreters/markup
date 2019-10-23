
CL?=sbcl --quit --eval
TMP:=$(shell tempfile)

test:
	$(CL) '(progn (ql:quickload "markup") (asdf:test-system "markup.test"))' 2>&1 | tee $(TMP)
	grep "Fail: 0 " $(TMP)
	rm -f $(TMP)
