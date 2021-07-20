getint:
  li $v0,5
  syscall
  jr $ra
  
getch:
  li $v0,12
  syscall
  jr $ra
  
putint:
  li $v0,1
  syscall
  jr $ra
  
putch:
  li $v0,11
  syscall
  jr $ra
  
getarray:
  jal getint
  move $t0,$v0
  move $t1,$zero
  j $getarray_cond
$getarray_cond:
  bne $t1,$t0,$getarray_body
  beq $t1,$t0,$getarray_end
$getarray_body:
  jal getint
  mul $t2,$t1,4
  add $t3,$a0,$t2
  sw $v0,($t3)
  add $t1,$t1,1
  j $getarray_cond
$getarray_end:
  move $v0,$t0
  jr $ra
  
putarray:
  li $v0,1
  syscall
  move $t0,$a0
  move $t1,$zero
  j $putarray_cond
$putarray_cond:
  bne $t1,$t0,$putarray_body
  beq $t1,$t0,$putarray_end
$putarray_body:
  mul $t2,$t1,4
  add $t3,$a1,$t2
  lw $a0,($t3)
  li $v0,1
  syscall
  add $t1,$t1,1
  j $putarray_cond
$putarray_end:
  jr $ra
  
starttime:
  jr $ra

stoptime:
  jr $ra