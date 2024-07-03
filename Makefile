c: target/c/out.c
	gcc -o target/c/out target/c/out.c

runc: c
	./target/c/out

viewc: target/c/out.c
	${PAGER} target/c/out.c