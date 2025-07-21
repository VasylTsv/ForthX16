!macro PGZ_HEADER {
	* = $0000
	!byte 'Z'
}

!macro SEGMENT .start, .end {
	!word .start
	!byte 0
	!word .end-.start
	!byte 0
}

!macro STARTUP .addr {
	!word .addr
	!fill 4,0
}
