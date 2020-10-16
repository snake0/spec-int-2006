$benchnum  = '403';
$benchname = 'gcc';
$exename   = 'gcc';
$benchlang = 'C';
@base_exe  = ($exename);

@sources=qw(
            alloca.c asprintf.c vasprintf.c
            c-parse.c c-lang.c attribs.c c-errors.c c-lex.c c-pragma.c 
            c-decl.c c-typeck.c c-convert.c c-aux-info.c c-common.c 
            c-format.c c-semantics.c c-objc-common.c main.c cpplib.c 
            cpplex.c cppmacro.c cppexp.c cppfiles.c cpphash.c cpperror.c 
            cppinit.c cppdefault.c line-map.c mkdeps.c prefix.c version.c 
            mbchar.c alias.c bb-reorder.c bitmap.c builtins.c caller-save.c 
            calls.c cfg.c cfganal.c cfgbuild.c cfgcleanup.c cfglayout.c 
            cfgloop.c cfgrtl.c combine.c conflict.c convert.c cse.c 
            cselib.c dbxout.c debug.c dependence.c df.c diagnostic.c 
            doloop.c dominance.c dwarf2asm.c dwarf2out.c dwarfout.c 
            emit-rtl.c except.c explow.c expmed.c expr.c final.c flow.c 
            fold-const.c function.c gcse.c genrtl.c ggc-common.c global.c 
            graph.c haifa-sched.c hash.c hashtable.c hooks.c ifcvt.c 
            insn-attrtab.c insn-emit.c insn-extract.c insn-opinit.c 
            insn-output.c insn-peep.c insn-recog.c integrate.c intl.c 
            jump.c  langhooks.c lcm.c lists.c local-alloc.c loop.c 
            obstack.c optabs.c 
            params.c predict.c print-rtl.c print-tree.c profile.c real.c 
            recog.c reg-stack.c regclass.c regmove.c regrename.c reload.c 
            reload1.c reorg.c resource.c rtl.c rtlanal.c rtl-error.c 
            sbitmap.c sched-deps.c sched-ebb.c sched-rgn.c sched-vis.c 
            sdbout.c sibcall.c simplify-rtx.c ssa.c ssa-ccp.c ssa-dce.c 
            stmt.c stor-layout.c stringpool.c timevar.c toplev.c tree.c 
            tree-dump.c tree-inline.c unroll.c varasm.c varray.c 
            vmsdbgout.c xcoffout.c ggc-page.c i386.c xmalloc.c xexit.c 
            hashtab.c safe-ctype.c splay-tree.c xstrdup.c md5.c fibheap.c 
            xstrerror.c concat.c partition.c hex.c lbasename.c getpwd.c
	    ucbqsort.c
           );
$need_math='yes';
$bench_flags='-I.';

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;
    my $exe  = $me->exe_file;

    for ($me->input_files_base) {
	if (($name) = m/(.*).i$/) {
	    push (@rc, { 'command' => $exe, 
			 'args'    => [ $_, "-o $name.s" ], 
			 'output'  => "$name.out",
			 'error'   => "$name.err",
			});
	}
    }
    @rc;
}

1;
