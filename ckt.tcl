#!/usr/bin/wish
# colors and sizes of user interface elements
set g_gui(dotsize) 8
set g_gui(boxsize) 20
set g_gui(margin) 30
set g_gui(grid1color) "red"
set g_gui(grid2color) "#6688ff"
set g_gui(boxcolor) "gray30"
set g_gui(background) "gray20"
set g_gui(breakcolor) "gray70"
set g_gui(rubberband) "yellow"

# definition of the Postscript page (US letter, 1-inch margins,
# 1/2-inch margin around EPS)
set g_ps(bottom) 72
set g_ps(top) 720
set g_ps(left) 72
set g_ps(right) 540
set g_ps(margin) 36

# knot drawing styles
set g_ps(styles) [list "ribbon" "simple" "dual" "abut"]
set g_ps(style,ribbon) "/w .4 def /s 0 def /d 0.05 def"
set g_ps(style,simple) "/w .3 def /s .1 def /d 0 def"
set g_ps(style,dual) "/w .15 def /s .1 def /d 0.15 def /edgecolor { 0.7 0.8 1 setrgbcolor } bind def /monocolor { 1 setgray } bind def"
set g_ps(style,abut) "/w .3 def /s 0 def /d 0 def"

# default file names
set g_misc(startps) "start.ps"
set g_misc(outputps) "output.eps"

set g_nextwindow 0

# solve and output the postscript knot for editor $t
proc generate_postscript {t} {
    global g_info g_ps g_misc

    set w $g_info($t,w)
    set h $g_info($t,h)
    
    if {[catch {open $g_ps($t,psname) "w"} fd]} {
	error "couldn't open $g_ps($t,psname)"
	return
    }

    set stuff [compute_layout $t]

    puts $fd "%!PS-Adobe-3.0 EPSF-3.0"
    puts $fd "%%BoundingBox: [lindex $stuff 1]"
    puts $fd "%%Creator: celtic knot thingy (http://www.cs.washington.edu/homes/dougz/)"
    puts $fd ""

    puts $fd $g_ps(style,$g_ps($t,style))
    
    if {![catch {open $g_misc(startps)} sfd]} {
	# skip two input lines
	gets $sfd line
	gets $sfd line

	while {![eof $sfd]} {
	    gets $sfd line
	    if {[regexp {^%%STOPCOPYHERE} $line]} break
	    puts $fd $line
	}
	close $sfd
    }

    puts $fd [lindex $stuff 0]

    solve_knot $t

    if {$g_ps($t,square)} {
	puts $fd "/knoxels squareknoxels def"
    } else {
	puts $fd "/knoxels roundknoxels def"
    }
    
    if {$g_ps($t,monochrome)} {
	puts $fd "$g_info($t,colors) monochrome"
    } else {
	puts $fd "$g_info($t,colors) color"
    }
    
    for {set j 0} {$j < $h} {incr j} {
	set rj [expr $h-$j-1]
	for {set i 0} {$i < $w} {incr i} {
	    if {$g_info($t,b,$i,$j)} {
		puts $fd "$i $rj $g_info($t,k,$i,$j) $g_info($t,pc,$i,$j) $g_info($t,sc,$i,$j) drawknoxel"
	    }
	}
    }

    if {$g_ps($t,gridlines)} {
	puts $fd "$w $h drawgrid"
    }

    if {$g_ps($t,breaks)} {
	puts $fd "gsave 0.25 setgray 0.05 setlinewidth newpath"
	for {set j 0} {$j <= $h} {incr j} {
	    set rj [expr $h-$j]
	    for {set i 0} {$i <= $w} {incr i} {
		if {$g_info($t,v,$i,$j)} {
		    puts $fd "$i $rj moveto 0 -1 rlineto"
		}
		if {$g_info($t,h,$i,$j)} {
		    puts $fd "$i $rj moveto 1 0 rlineto"
		}
	    }
	}
	puts $fd "stroke grestore"
    }
    if {$g_ps($t,markers)} {
	puts $fd "0.5 setgray"
	for {set j 0} {$j <= $h} {incr j 2} {
	    set rj [expr $h-$j]
	    for {set i 0} {$i <= $w} {incr i 2} {
		puts $fd "$i $rj marker"
	    }
	}
	puts $fd "0.8 setgray"
	for {set j 1} {$j <= $h} {incr j 2} {
	    set rj [expr $h-$j]
	    for {set i 1} {$i <= $w} {incr i 2} {
		puts $fd "$i $rj marker"
	    }
	}
    }
    
    puts $fd "showpage"
    puts $fd "%%EOF"

    close $fd
}

# fit knot on the page and compute EPS bounding box.
proc compute_layout {t} {
    global g_info g_ps

    set w $g_info($t,w)
    set h $g_info($t,h)

    if {(double($h)/$w) > (double($g_ps(top)-$g_ps(bottom))/($g_ps(right)-$g_ps(left)))} {
	# fit top-to-bottom, center vertically
	set xform "$g_ps(left) $g_ps(right) add $w $h div $g_ps(top) $g_ps(bottom) sub mul sub 2 div   $g_ps(bottom) translate\n"
	append xform "$g_ps(top) $g_ps(bottom) sub $h div dup scale\n"

	set bbbottom $g_ps(bottom)
	set bbtop $g_ps(top)
	set bbleft [expr ($g_ps(left)+$g_ps(right)-(double($w)/$h*($g_ps(top)-$g_ps(bottom))))/2]
	set bbright [expr ($g_ps(left)+$g_ps(right)+(double($w)/$h*($g_ps(top)-$g_ps(bottom))))/2]
    } else {
	# fit left-to-right, center horizontally
	set xform "$g_ps(left)   $g_ps(top) $g_ps(bottom) add $h $w div $g_ps(right) $g_ps(left) sub mul sub 2 div translate\n"
	append xform "$g_ps(right) $g_ps(left) sub $w div dup scale\n"

	set bbleft $g_ps(left)
	set bbright $g_ps(right)
	set bbbottom [expr ($g_ps(bottom)+$g_ps(top)-(double($h)/$w*($g_ps(right)-$g_ps(left))))/2]
	set bbtop [expr ($g_ps(bottom)+$g_ps(top)+(double($h)/$w*($g_ps(right)-$g_ps(left))))/2]
    }

    set bbleft [expr int($bbleft)-$g_ps(margin)]
    set bbbottom [expr int($bbbottom)-$g_ps(margin)]
    set bbright [expr int($bbright+0.99)+$g_ps(margin)]
    set bbtop [expr int($bbtop+0.99)+$g_ps(margin)]

    return [list $xform "$bbleft $bbbottom $bbright $bbtop"]
}

proc solve_knot {t} {
    global g_info 

    set w $g_info($t,w)
    set h $g_info($t,h)
    set rev $g_info($t,reverse)

    # masterlist encodes how breaks are turned into knoxels.  let the
    # system of breaks around a single square be encoded as the sum
    # of:
    #
    #    +--2--+
    #    |     |
    #    1     4
    #    |     |
    #    +--8--+
    #
    # use the first set of 16 elements if the location has even parity
    # (i+j, markers at nw and se) or the second set for odd parity
    # (markers at sw and ne).  of each pair, use the first element for
    # odd columns or the second for even columns (or switch this, to
    # reverse the interlacing pattern.  rows could also be used; the
    # result is the same.
    #
    # -1 represents situations that should never happen (an illegal
    # set of breaks).
    
    set masterlist [list \
	    [list \
	    { 0  2} {13 17} {24 20} { 8  8} \
	    {15 11} { 4  4} {-1 -1} {-1 -1} \
	    {18 22} {-1 -1} { 5  5} {-1 -1} \
	    { 6  6} {-1 -1} {-1 -1} {-1 -1}] \
	    [list \
	    { 3  1} {23 19} {14 10} {-1 -1} \
	    {21 25} { 4  4} { 9  9} {-1 -1} \
	    {12 16} { 7  7} { 5  5} {-1 -1} \
	    {-1 -1} {-1 -1} {-1 -1} {-1 -1}]]

    set c 0
    for {set j 0} {$j < $h} {incr j} {
	for {set i 0} {$i < $w} {incr i} {
	    set breakcode 0
	    if {$g_info($t,v,$i,$j)} { incr breakcode 1 }
	    if {$g_info($t,h,$i,$j)} { incr breakcode 2 }
	    if {$g_info($t,v,[expr $i+1],$j)} { incr breakcode 4 }
	    if {$g_info($t,h,$i,[expr $j+1])} { incr breakcode 8 }
	    
	    set g_info($t,k,$i,$j) [lindex [lindex [lindex $masterlist [expr ($i+$j)%2]] $breakcode] [expr ($i%2)^$rev]]
	    #puts -nonewline [format "%4d" $g_info($t,k,$i,$j)]

	    # primary color
	    set g_info($t,pc,$i,$j) $c
	    set parent($c) -1
	    incr c
	}
	#puts ""
    }
    #puts ""

    #
    # now compute primary colors.  initially each knoxel has its own
    # primary color (assigned above).  for each knoxel K, we look at
    # four adjacent knoxels:
    #
    #       K - h
    #     / | \
    #   bd  v  fd
    #
    # if the particular knoxel K connects to any of these, we merge
    # the two primary colors (using a union-find data structure).
    # after all knoxels are processed the result will be a partition
    # of knoxels into strands.
    #
    
    for {set j 0} {$j < $h} {incr j} {
	for {set i 0} {$i < $w} {incr i} {
	    
	    switch -- $g_info($t,k,$i,$j) {
		1 - 3 - 10 - 14 - 19 - 23 {
		    # merge forward diag
		    if {$i < $w-1 && $j < $h-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,[expr $i+1],[expr $j+1])
		    }
		}
		0 - 2 - 11 - 15 {
		    # merge backward diag
		    if {$i > 0 && $j < $h-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,[expr $i-1],[expr $j+1])
		    }
		}
		5 - 7 - 12 - 16 {
		    # merge horz
		    if {$i < $w-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,[expr $i+1],$j)
		    }
		}
		4 - 9 - 13 - 17 - 21 - 25 {
		    # merge vert
		    if {$j < $h-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,$i,[expr $j+1])
		    }
		}
		8 {
		    # merge horz
		    if {$i < $w-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,[expr $i+1],$j)
		    }
		    # and merge vert
		    if {$j < $h-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,$i,[expr $j+1])
		    }
		}
		20 - 24 {
		    # merge horz
		    if {$i < $w-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,[expr $i+1],$j)
		    }
		    # and merge backward diag
		    if {$i > 0 && $j < $h-1} {
			uf_union parent $g_info($t,pc,$i,$j) $g_info($t,pc,[expr $i-1],[expr $j+1])
		    }
		}
		    
	    }
	}
    }

    # now compute the partition, and remap the color numbers onto
    # 0..N-1, where N is the number of strands
    
    set nc 0
    for {set k 0} {$k < $c} {incr k} {
	if {$parent($k)==-1} {
	    set remap($k) $nc
	    incr nc
	}
    }
    for {set j 0} {$j < $h} {incr j} {
	for {set i 0} {$i < $w} {incr i} {
	    if {$parent($g_info($t,pc,$i,$j))==-1} {
		set g_info($t,pc,$i,$j) $remap($g_info($t,pc,$i,$j))
	    } else { 
		set g_info($t,pc,$i,$j) $remap([uf_find parent $g_info($t,pc,$i,$j)])
	    }
	    #puts -nonewline [format "%4d" $g_info($t,pc,$i,$j)]
	}
	#puts ""
    }
    #puts ""

    #
    # now compute secondary colors by taking primary colors of adjacents.
    #

    # the direction table looks like this: "sw" in position 15 means
    # that if I am knoxel 15, my secondary color is the primary color
    # of the knoxels to the south and the west (which should be the
    # same, if they both exist).  if neither exists, a new color is
    # assigned.
    #
    # "xx" means that a knoxel does not have a secondary color.
    
    set direction [list \
	    sw nw ne se \
	    xx xx xx xx xx xx \
	    xx xx xx xx \
	    se sw nw ne \
	    xx xx xx xx \
	    ne se sw nw]
    
    for {set j 0} {$j < $h} {incr j} {
	for {set i 0} {$i < $w} {incr i} {
	    switch -- [lindex $direction $g_info($t,k,$i,$j)] {
		xx {
		    set g_info($t,sc,$i,$j) 0
		}
		nw {
		    if {$i > 0} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,[expr $i-1],$j)
		    } elseif {$j > 0} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,$i,[expr $j-1])
		    } else {
			set g_info($t,sc,$i,$j) $nc
			incr nc
		    }
		}
		ne {
		    if {$i < $w-1} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,[expr $i+1],$j)
		    } elseif {$j > 0} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,$i,[expr $j-1])
		    } else {
			set g_info($t,sc,$i,$j) $nc
			incr nc
		    }
		}
		sw {
		    if {$i > 0} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,[expr $i-1],$j)
		    } elseif {$j < $h-1} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,$i,[expr $j+1])
		    } else {
			set g_info($t,sc,$i,$j) $nc
			incr nc
		    }
		}
		se {
		    if {$i < $w-1} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,[expr $i+1],$j)
		    } elseif {$j < $h-1} {
			set g_info($t,sc,$i,$j) $g_info($t,pc,$i,[expr $j+1])
		    } else {
			set g_info($t,sc,$i,$j) $nc
			incr nc
		    }
		}
	    }
	}
    }

    set g_info($t,colors) $nc
}

proc uf_union {parentvar from to} {
    upvar $parentvar parent

    while {$parent($to)!=-1} { set to $parent($to) }
    while {$parent($from)!=-1} { set from $parent($from) }
    if {$to != $from} {
	set parent($to) $from
    }
}

proc uf_find {parentvar start} {
    upvar $parentvar parent
    set lyst {}
    while {$parent($start)!=-1} {
	lappend lyst $start
	set start $parent($start)
    }
    foreach item $lyst {
	set parent($item) $start
    }
    return $start
}
	

proc reset_editor {t {redraw 0}} {
    global g_info

    set w $g_info($t,w)
    set h $g_info($t,h)
    
    # initialize all boxes to ON and set up breaks around the edge.
    for {set j 0} {$j <= $h} {incr j} {
	for {set i 0} {$i <= $w} {incr i} {
	    set g_info($t,b,$i,$j) 1
	    set g_info($t,h,$i,$j) [expr ($j==0||$j==$h)&&($i<$w)]
	    set g_info($t,v,$i,$j) [expr ($i==0||$i==$w)&&($j<$h)]
	}
    }
    
    # remove illegal breaks at corners (for panels with odd width or height).
    if {$w % 2} {
	set g_info($t,h,[expr $w-1],0) 0
	set g_info($t,v,$w,0) 0
    }
    if {$h % 2} {
	set g_info($t,v,0,[expr $h-1]) 0
	set g_info($t,h,0,$h) 0
    }
    if {($w+$h) % 2} {
	set g_info($t,v,$w,[expr $h-1]) 0
	set g_info($t,h,[expr $w-1],$h) 0
    }

    if {$redraw} {
	redraw_boxes $t
	redraw_breaks $t
    }
}

proc knot_panel {t {args}} {
    global g_info g_ps g_misc

    if {[llength $args]==2} {
	set w [lindex $args 0]
	set h [lindex $args 1]
	
	set g_info($t,w) $w
	set g_info($t,h) $h

	reset_editor $t
    } elseif {[llength $args]==1} {
	if {[load_knot $t [lindex $args 0]]} return
	
	set w $g_info($t,w)
	set h $g_info($t,h)
    }

    toplevel $t

    # draw the canvas
    initial_canvas $t

    set g_ps($t,psname) $g_misc(outputps)
    
    frame $t.f1
    button $t.f1.ps -text "postscript" -command [list generate_postscript $t]
    entry $t.f1.psname -width 20 -textvariable g_ps($t,psname)
    button $t.f1.psnamebrowse -text "browse..." -command [list browse_savename $t g_ps($t,psname)]
    button $t.f1.reset -text "reset" -command [list reset_editor $t true]
    button $t.f1.save -text "save" -command [list save_knot $t]
    button $t.f1.close -text "close" -command [list destroy $t]
    pack $t.f1.ps -padx 10 -side left
    pack $t.f1.psname $t.f1.psnamebrowse -side left
    pack $t.f1.close $t.f1.save $t.f1.reset -padx 5 -side right

    set g_ps($t,gridlines) 0
    set g_ps($t,breaks) 0
    set g_ps($t,markers) 0
    set g_ps($t,monochrome) 1
    set g_ps($t,square) 0
    set g_ps($t,style) [lindex $g_ps(styles) 0]

    frame $t.f2
    checkbutton $t.f2.cells -text "knoxel cells" -variable g_ps($t,gridlines)
    checkbutton $t.f2.breaks -text "breaks" -variable g_ps($t,breaks)
    checkbutton $t.f2.markers -text "markers" -variable g_ps($t,markers)
    pack $t.f2.cells $t.f2.breaks $t.f2.markers -side left
    
    frame $t.f3
    checkbutton $t.f3.monochrome -text "monochrome" -variable g_ps($t,monochrome)
    checkbutton $t.f3.square -text "square corners" -variable g_ps($t,square)
    
    menubutton $t.f3.style -text "style" -menu $t.f3.style.m -relief raised
    menu $t.f3.style.m
    foreach item $g_ps(styles) {
	$t.f3.style.m add radiobutton -label "$item" -value $item -variable g_ps($t,style)
    }
    $t.f3.style.m add separator
    set g_info($t,reverse) 0
    $t.f3.style.m add checkbutton -label "reverse interlace" -onvalue 1 -offvalue 0 -variable g_info($t,reverse)
    
    pack $t.f3.monochrome $t.f3.square $t.f3.style -side left
    
    pack $t.f1 $t.f2 $t.f3 -fill x
}

proc browse_savename {t var} {
    upvar $var filename

    set foo [tk_getSaveFile -defaultextension ".eps" \
	    -filetypes {{"PostScript files" {.ps .eps}} {"All files" {*}}} \
	    -initialdir [file dirname $filename] \
	    -initialfile [file tail $filename] -parent $t \
	    -title "Set Postscript output file"]
    if {$foo == ""} return
    set filename $foo
}

proc save_knot {t} {
    global g_info
    
    set fn [tk_getSaveFile -defaultextension ".kn" \
	    -filetypes {{"Knots" {.kn}} {"All files" {*}}} \
	    -parent $t -title "Save knot as"]
    if {$fn == ""} return

    if {[catch {open $fn w} fd]} {
	error "can't open file $fn"
	return
    }

    puts $fd [list $g_info($t,w) $g_info($t,h)]
    
    for {set j 0} {$j <= $g_info($t,h)} {incr j} {
	set output {}
	for {set i 0} {$i <= $g_info($t,w)} {incr i} {
	    lappend output [expr !!$g_info($t,b,$i,$j)]
	}
	puts $fd $output
    }
    
    for {set j 0} {$j <= $g_info($t,h)} {incr j} {
	set output {}
	for {set i 0} {$i <= $g_info($t,w)} {incr i} {
	    lappend output [list [expr !!$g_info($t,h,$i,$j)] [expr !!$g_info($t,v,$i,$j)]]
	}
	puts $fd $output
    }

    close $fd
}

proc load_knot {t fn} {
    global g_info
    
    if {[catch {open $fn} fd]} {
	error "can't open file $fn"
	return -1
    }

    gets $fd line
    set g_info($t,w) [lindex $line 0]
    set g_info($t,h) [lindex $line 1]
    puts "knot is $line"

    for {set j 0} {$j <= $g_info($t,h)} {incr j} {
	gets $fd line
	set i 0
	foreach item $line {
	    set g_info($t,b,$i,$j) $item
	    incr i
	}
    }
    puts "loaded boxes"

    for {set j 0} {$j <= $g_info($t,h)} {incr j} {
	gets $fd line
	set i 0
	foreach item $line {
	    set g_info($t,h,$i,$j) [lindex $item 0]
	    set g_info($t,v,$i,$j) [lindex $item 1]
	    incr i
	}
    }
    puts "loaded breaks"

    close $fd

    return 0
}

# erase all boxes and redraw them
proc redraw_boxes {t} {
    global g_gui g_info

    $t.c delete box
    
    for {set j 0} {$j < $g_info($t,h)} {incr j} {
	for {set i 0} {$i < $g_info($t,w)} {incr i} {
	    if {$g_info($t,b,$i,$j)} {
		set g_info($t,bid,$i,$j) [$t.c create rect \
			[expr $g_gui(margin) + $i*$g_gui(boxsize) + ($i+1)*$g_gui(dotsize)] \
			[expr $g_gui(margin) + $j*$g_gui(boxsize) + ($j+1)*$g_gui(dotsize)] \
			[expr $g_gui(margin) + ($i+1)*$g_gui(boxsize) + ($i+1)*$g_gui(dotsize)] \
			[expr $g_gui(margin) + ($j+1)*$g_gui(boxsize) + ($j+1)*$g_gui(dotsize)] \
			-fill $g_gui(boxcolor) -outline {} -tags box]
	    } else {
		set g_info($t,bid,$i,$j) [$t.c create rect \
			[expr $g_gui(margin) + $i*$g_gui(boxsize) + ($i+1)*$g_gui(dotsize)] \
			[expr $g_gui(margin) + $j*$g_gui(boxsize) + ($j+1)*$g_gui(dotsize)] \
			[expr $g_gui(margin) + ($i+1)*$g_gui(boxsize) + ($i+1)*$g_gui(dotsize)] \
			[expr $g_gui(margin) + ($j+1)*$g_gui(boxsize) + ($j+1)*$g_gui(dotsize)] \
			-fill $g_gui(background) -outline {} -tags box]
	    } 
	}
    }

    $t.c lower box
}

# erase all break markers and redraw them
proc redraw_breaks {t} {
    global g_gui g_info

    $t.c delete break

    for {set j 0} {$j <= $g_info($t,h)} {incr j} {
	for {set i 0} {$i <= $g_info($t,w)} {incr i} {
	    if {$g_info($t,h,$i,$j)} {
		set g_info($t,h,$i,$j) [$t.c create line \
			[expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			[expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			[expr $g_gui(margin) + ($i+1)*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			[expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			-fill $g_gui(breakcolor) -width 2 -tags break]
	    }
	    if {$g_info($t,v,$i,$j)} {
		set g_info($t,v,$i,$j) [$t.c create line \
			[expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			[expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			[expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			[expr $g_gui(margin) + ($j+1)*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
			-fill $g_gui(breakcolor) -width 2 -tags break]
	    }
	}
    }

    $t.c raise break box
}

# draw a single break marker
proc draw_break {t d i j} {
    global g_info g_gui
    if {$d == "h"} {
	set g_info($t,$d,$i,$j) [$t.c create line \
		[expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		[expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		[expr $g_gui(margin) + ($i+1)*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		[expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		-fill $g_gui(breakcolor) -width 2 -tags break]
    } else {
	set g_info($t,$d,$i,$j) [$t.c create line \
		[expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		[expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		[expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		[expr $g_gui(margin) + ($j+1)*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2] \
		-fill $g_gui(breakcolor) -width 2 -tags break]
    }
}
    
# build the canvas for an editor panel, draw the initial state
proc initial_canvas {t} {
    global g_gui g_info

    set w $g_info($t,w)
    set h $g_info($t,h)

    frame $t.cc 
    
    set cw [expr $g_gui(margin)*2 + $w*$g_gui(boxsize) + ($w+1)*$g_gui(dotsize)]
    set ch [expr $g_gui(margin)*2 + $h*$g_gui(boxsize) + ($h+1)*$g_gui(dotsize)]
    if {$cw > 600} {
	set ww 600
    } else {
	set ww $cw
    }
    if {$ch > 600} {
	set wh 600
    } else {
	set wh $ch
    }
    canvas $t.c \
	    -width $ww -height $wh -scrollregion [list 0 0 $cw $ch] \
	    -background $g_gui(background)
    scrollbar $t.hs -orient h -command "$t.c xview"
    scrollbar $t.vs -orient v -command "$t.c yview"
    $t.c config -xscrollcommand "$t.hs set" -yscrollcommand "$t.vs set"

    grid $t.c -in $t.cc -row 0 -column 0 -sticky nsew
    grid $t.hs -in $t.cc -row 1 -column 0 -sticky ew
    grid $t.vs -in $t.cc -row 0 -column 1 -sticky ns
    grid columnconfigure $t.cc 0 -weight 1
    grid rowconfigure $t.cc 0 -weight 1
    pack $t.cc -fill both -expand true

    for {set j 0} {$j <= $h} {incr j 2} {
	for {set i 0} {$i <= $w} {incr i 2} {
	    $t.c create rect \
		    [expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize))] \
		    [expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize))] \
		    [expr $g_gui(margin) + $i*$g_gui(boxsize) + ($i+1)*$g_gui(dotsize)] \
		    [expr $g_gui(margin) + $j*$g_gui(boxsize) + ($j+1)*$g_gui(dotsize)] \
		    -fill $g_gui(grid1color) -outline {} -tags dot
	}
    }
	    
    for {set j 1} {$j <= $h} {incr j 2} {
	for {set i 1} {$i <= $w} {incr i 2} {
	    $t.c create rect \
		    [expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize))] \
		    [expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize))] \
		    [expr $g_gui(margin) + $i*$g_gui(boxsize) + ($i+1)*$g_gui(dotsize)] \
		    [expr $g_gui(margin) + $j*$g_gui(boxsize) + ($j+1)*$g_gui(dotsize)] \
		    -fill $g_gui(grid2color) -outline {} -tags dot
	}
    }

    redraw_boxes $t
    redraw_breaks $t

    $t.c bind dot <ButtonPress-1> "start_break 1 $t %x %y"
    $t.c bind dot <Control-ButtonPress-1> "start_break 0 $t %x %y"
    $t.c bind dot <ButtonPress-3> "start_break 0 $t %x %y"

    $t.c bind box <ButtonPress-3> "toggle_box $t %x %y"
    $t.c bind box <ButtonPress-1> "flood_box $t %x %y"
}

# flood_box toggles the selected box, and sets all boxes in its
# connected region of the diagram to the same state.
proc flood_box {t sx sy} {
    global g_info g_gui

    set x [$t.c canvasx $sx]
    set y [$t.c canvasy $sy]

    set spacing [expr $g_gui(boxsize)+$g_gui(dotsize)]
    set si [expr int(($x-$g_gui(margin)-$g_gui(dotsize)/2)/$spacing)]
    set sj [expr int(($y-$g_gui(margin)-$g_gui(dotsize)/2)/$spacing)]

    set new [expr !$g_info($t,b,$si,$sj)]

    set w $g_info($t,w)
    set h $g_info($t,h)
    
    for {set j 0} {$j < $h} {incr j} {
	for {set i 0} {$i < $w} {incr i} {
	    set parent([expr $j*$w+$i]) -1
	}
    }
    for {set j 0} {$j < $h} {incr j} {
	for {set i 0} {$i < $w} {incr i} {
	    if {$i > 0 && !$g_info($t,v,$i,$j)} {
		uf_union parent [expr $j*$w+$i] [expr $j*$w+($i-1)]
	    }
	    if {$j > 0 && !$g_info($t,h,$i,$j)} {
		uf_union parent [expr $j*$w+$i] [expr ($j-1)*$w+$i]
	    }
	}
    }
    set target [uf_find parent [expr $sj*$w+$si]]
    if {$new} {
	for {set j 0} {$j < $h} {incr j} {
	    for {set i 0} {$i < $w} {incr i} {
		if {[uf_find parent [expr $j*$w+$i]] == $target && $g_info($t,b,$i,$j) != $new} {
		    $t.c itemconfig $g_info($t,bid,$i,$j) -fill $g_gui(boxcolor)
		    set g_info($t,b,$i,$j) 1
		}
	    }
	}
    } else {
	for {set j 0} {$j < $h} {incr j} {
	    for {set i 0} {$i < $w} {incr i} {
		if {[uf_find parent [expr $j*$w+$i]] == $target && $g_info($t,b,$i,$j) != $new} {
		    $t.c itemconfig $g_info($t,bid,$i,$j) -fill $g_gui(background)
		    set g_info($t,b,$i,$j) 0
		}
	    }
	}
    }
}

# toggle_box toggles a single box.
proc toggle_box {t sx sy} {
    global g_info g_gui
    
    set x [$t.c canvasx $sx]
    set y [$t.c canvasy $sy]

    set spacing [expr $g_gui(boxsize)+$g_gui(dotsize)]
    set i [expr int(($x-$g_gui(margin)-$g_gui(dotsize)/2)/$spacing)]
    set j [expr int(($y-$g_gui(margin)-$g_gui(dotsize)/2)/$spacing)]

    if {$g_info($t,b,$i,$j)} {
	$t.c itemconfig $g_info($t,bid,$i,$j) -fill $g_gui(background)
	set g_info($t,b,$i,$j) 0
    } else {
	$t.c itemconfig $g_info($t,bid,$i,$j) -fill $g_gui(boxcolor)
	set g_info($t,b,$i,$j) 1
    }
	
}
    
# start_break starts a rubberband line at a marker
proc start_break {mode t sx sy} {
    global g_info g_gui g_rubberband

    if {[info exists g_rubberband]} return

    set x [$t.c canvasx $sx]
    set y [$t.c canvasy $sy]

    set spacing [expr $g_gui(boxsize)+$g_gui(dotsize)]
    set i [expr int(($x-$g_gui(margin)+$g_gui(dotsize)/2)/$spacing)]
    set j [expr int(($y-$g_gui(margin)+$g_gui(dotsize)/2)/$spacing)]

    set g_rubberband(si) $i
    set g_rubberband(sj) $j
    set g_rubberband(ei) $i
    set g_rubberband(ej) $j
    set g_rubberband(sx) [expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2]
    set g_rubberband(sy) [expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2]
    $t.c create line $g_rubberband(sx) $g_rubberband(sy) $g_rubberband(sx) $g_rubberband(sy) \
	    -fill $g_gui(rubberband) -width 2 -tags rubber

    bind $t.c <Motion> "drag_break $mode $t %x %y"
    bind $t.c <ButtonRelease> "finish_break $mode $t %x %y"
}

# drag_break updates the rubberband line when the mouse moves
proc drag_break {mode t sx sy} {
    global g_info g_gui g_rubberband

    set x [$t.c canvasx $sx]
    set y [$t.c canvasy $sy]

    set dx [expr abs($g_rubberband(sx)-$x)]
    set dy [expr abs($g_rubberband(sy)-$y)]

    set spacing [expr $g_gui(boxsize)+$g_gui(dotsize)]
    
    if {$dx > $dy} {
	# horizontal line

	set neg [expr $x < $g_rubberband(sx)]
	set i [expr int((abs($x-$g_rubberband(sx)))/$spacing)]
	set i [expr ($i-1)/2+1]
	if {$i < 1} {set i 1}
	if {$neg} {set i [expr -2*$i+$g_rubberband(si)]} else {set i [expr 2*$i+$g_rubberband(si)]}
	
	if {$i < 0} {
	    set i [expr $g_rubberband(si)%2]
	} elseif {$i > $g_info($t,w)} {
	    if {$g_info($t,w)%2 == $g_rubberband(si)%2} {
		set i $g_info($t,w)
	    } else {
		set i [expr $g_info($t,w)-1]
	    }
	}
	
	
	set j $g_rubberband(sj)

	set ex [expr $g_gui(margin) + $i*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2]
	set ey $g_rubberband(sy)
    } else {
	# vertical line
    
	set i $g_rubberband(si)

	set neg [expr $y < $g_rubberband(sy)]
	set j [expr int((abs($y-$g_rubberband(sy)))/$spacing)]
	set j [expr ($j-1)/2+1]
	if {$j < 1} {set j 1}
	if {$neg} {set j [expr -2*$j+$g_rubberband(sj)]} else {set j [expr 2*$j+$g_rubberband(sj)]}

	if {$j < 0} {
	    set j [expr $g_rubberband(sj)%2]
	} elseif {$j > $g_info($t,h)} {
	    if {$g_info($t,h)%2 == $g_rubberband(sj)%2} {
		set j $g_info($t,h)
	    } else {
		set j [expr $g_info($t,h)-1]
	    }
	}

	set ex $g_rubberband(sx)
	set ey [expr $g_gui(margin) + $j*($g_gui(boxsize)+$g_gui(dotsize)) + $g_gui(dotsize)/2]
    }

    $t.c coords rubber $g_rubberband(sx) $g_rubberband(sy) $ex $ey
    set g_rubberband(ei) $i
    set g_rubberband(ej) $j
}

# finish_break completes the rubberband operation, and adds or deletes
# the user's breakpoints accordingly.
proc finish_break {mode t sx sy} {
    global g_info g_gui g_rubberband
    
    drag_break $mode $t $sx $sy

    set si $g_rubberband(si)
    set sj $g_rubberband(sj)
    set ei $g_rubberband(ei)
    set ej $g_rubberband(ej)

    if {$si != $ei} {
	
	if {$si > $ei} {
	    set temp $si
	    set si $ei
	    set ei $temp
	}

	if {$mode} {
	    for {set i $si} {$i < $ei} {incr i} {
		if {!$g_info($t,h,$i,$sj)} {
		    draw_break $t h $i $sj
		}
	    }
	} else {
	    for {set i $si} {$i < $ei} {incr i} {
		$t.c delete $g_info($t,h,$i,$sj)
		set g_info($t,h,$i,$sj) 0
	    }
	}

	# delete breaks around illegal crossings
	for {set i $si} {$i < $ei} {incr i} {
	    if {$sj > 0} { fixup_breaks $t $i [expr $sj-1] h }
	    if {$sj < $g_info($t,h)} { fixup_breaks $t $i $sj h }
	}
    } elseif {$sj != $ej} {
	if {$sj > $ej} {
	    set temp $sj
	    set sj $ej
	    set ej $temp
	}

	if {$mode} {
	    for {set j $sj} {$j < $ej} {incr j} {
		if {!$g_info($t,v,$si,$j)} {
		    draw_break $t v $si $j
		}
	    }
	} else {
	    for {set j $sj} {$j < $ej} {incr j} {
		$t.c delete $g_info($t,v,$si,$j)
		set g_info($t,v,$si,$j) 0
	    }
	}
	    
	# delete breaks around illegal crossings
	for {set j $sj} {$j < $ej} {incr j} {
	    if {$si > 0} { fixup_breaks $t [expr $si-1] $j v }
	    if {$si < $g_info($t,w)} { fixup_breaks $t $si $j v }
	}
    }

    $t.c raise break box

    unset g_rubberband
    
    bind $t.c <Motion> {}
    bind $t.c <ButtonRelease> {}
    $t.c delete rubber
}

# fixup_breaks checks the break structure at box (i,j).  if it's
# illegal, the break not in the "pref" direction is deleted.
proc fixup_breaks {t i j pref} {
    global g_info

    if {($i+$j)%2} {
	# odd box can't have both h and v, remove non-preferred
	if {$g_info($t,h,$i,$j) && $g_info($t,v,$i,$j)} {
	    if {$pref == "h"} {
		$t.c delete $g_info($t,v,$i,$j) 
		set g_info($t,v,$i,$j) 0
	    } else {
		$t.c delete $g_info($t,h,$i,$j) 
		set g_info($t,h,$i,$j) 0
	    }
	}
    } else {
	# even box can't have h and v(i+1), or v and h(j+1)
	if {$g_info($t,h,$i,$j) && $g_info($t,v,[expr $i+1],$j)} {
	    if {$pref == "h"} {
		$t.c delete $g_info($t,v,[expr $i+1],$j)
		set g_info($t,v,[expr $i+1],$j) 0
	    } else {
		$t.c delete $g_info($t,h,$i,$j)
		set g_info($t,h,$i,$j) 0
	    }
	}
	
	if {$g_info($t,v,$i,$j) && $g_info($t,h,$i,[expr $j+1])} {
	    if {$pref == "h"} {
		$t.c delete $g_info($t,v,$i,$j)
		set g_info($t,v,$i,$j) 0
	    } else {
		$t.c delete $g_info($t,h,$i,[expr $j+1])
		set g_info($t,h,$i,[expr $j+1]) 0
	    }
	}
    }
}


scale .w -orient h -from 1 -to 200 -res 1 -label width -length 3i
scale .h -orient h -from 1 -to 200 -res 1 -label height -length 3i
.w set 16
.h set 16

frame .f
button .f.l -text "load" -command {
    set fn [tk_getOpenFile -defaultextension ".kn" \
	    -filetypes {{"Knots" {.kn}} {"All files" {*}}} \
	    -title "Open saved knot"]
    if {$fn != ""} {
	knot_panel .[incr g_nextwindow] $fn
    }
}

button .f.o -text "open window" -command {
    knot_panel .[incr g_nextwindow] [.w get] [.h get]
}
button .f.q -text quit -command exit
pack .f.l .f.o .f.q -side left

pack .w .h .f


    
