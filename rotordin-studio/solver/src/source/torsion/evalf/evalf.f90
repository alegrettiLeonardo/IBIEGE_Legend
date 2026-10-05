!
!     $Id$
!
!>    @file evalf.f
!>    @author francisco
!>    @brief evaluate text functions like t^2+3*t-cos(t).
!>     generate time and torque values based on time torque function
!>     using "t" as only time dependent variable.
!>    \verbatim
!> expression must fit on evaluator buffer, check out allocated size.
!> all regular math functions are handled by it signs like sum by "+",
!> subtraction by "-", product by "*", division by "/" and power by "^".
!> trigonometric functions: sin,cos,tan,asin,acos,atan,sinh,cosh,tanh.
!> other funcions:log,exp,abs,nint,aint,sqrt,log10,anint,floor.
!> "pi" number is also handled.
!>
!>Routine tests:
!>1. expression:
!>(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+
!>z*tan(z)*2/(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*
!>cos(x)+y*sin(y)+z*tan(z))*3+sqrt(x*y*z+x+y+z)*
!>log10(sqrt(x*2+y*2+z*2)+x+y+z))
!>for x = 0.175, y = 0.110 e z = 0.900 result is 5.481916.
!>
!>2. expression:
!>a+b*x1
!>for a = 0.900, b = 0.100 e x1 = 0.508, result is 0.9508000.
!>
!>3. expression:
!>cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*f)+154.3)
!>for x = 0.175, y = 0.110, z = 0.900, a = 0.900, c = 0.110,
!>d = 0.120, f = 0.140, x1 = 0.508 e x2 = 30.000, result is 20.69617.
!>
!>4. expression:
!>atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))
!>for x = 0.175, y = 0.110, z = 0.900, a = 0.900, c = 0.110,
!>f= 0.140 e e = 0.130, result is 1.559742.
!>
!>5. expression:
!>atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))*cos(log(abs(sqrt(y+a**
!>for x = 0.175, y = 0.110, z = 0.900, a = 0.900, c = 0.110,
!>f= 0.140 e e = 0.130, result is 1.557368.
!>
!>    \endverbatim
!>    changes:<br>
!>    new module - francisco - 19-nov-20<br>
!>    added param std on tgntp - francisco feb-21.
!>    \verbatim
!>"AN EXPRESSION EVALUATOR IN FORTRAN",
!> Wilton Pereira da Silva - Federal University of Campina Grande,2005.
!>
!> +-------------------------+
!> | RECURSIVE CALLS INSIDE  |
!> | not strictly Fortran 77 |
!> +-------------------------+
!>
!>    \endverbatim
!
!>    @brief rocognize variables.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param itoke number of tokens
!>    @param[in,out] iope number of operations
!>    @param mxvarnms dimension of variables vector
!>    @param mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param[in,out] oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
subroutine recvars(nn,ntokens,inumbr,itoke,iope,&
&mxvarnms,mxstokens,numbr,toke,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, mxvarnms, mxstokens, oper
  character(len=*) :: toke, stokens, varnms, cmsg
  dimension numbr(mxstokens),oper(mxstokens),&
  &varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!

  integer :: i, ierror
  character(len=7) :: op
  character(len=35) :: cms
!
  dimension cms(3)
!
  parameter (op = '+-*/^()',&
  &cms = (/'recvars:expression has an error ',&
  &'recvars:oper vector out of bound',&
  &'recvars:stokens vec.out of bound'/))
!
  intrinsic :: index, trim
!
  stat = .false.
!
  if (index(op,trim(toke)) .ne. 0) then
    cmsg = cms(1)
    return
  end if
!
  do i = 1,nn
!       variable
    if (trim(toke) .eq. varnms(i)) then
!         check bound
      if (iope .gt. mxstokens) then
        cmsg = cms(2)
        return
      end if
      oper(iope) = 28+i
      iope = iope+1
      if (itoke .lt. ntokens) then
        itoke = itoke+1
!           check bound
        if (itoke .gt. mxstokens) then
          cmsg = cms(3)
          return
        end if
        toke = stokens(itoke)
      end if
      stat = .true.
      return
    end if
  end do
!
!     number
  toke = trim(toke)
  read(toke, *, iostat = ierror) numbr(inumbr)
  if (ierror .ne. 0) then
    cmsg = cms(1)
    return
  else
!       check bound
    if (iope .gt. mxstokens) then
      cmsg = cms(2)
      return
    end if
    oper(iope) = 1
    iope = iope+1
    if (itoke .lt. ntokens) then
      itoke = itoke+1
!         check bound
      if (itoke .gt. mxstokens) then
        cmsg = cms(3)
        return
      end if
      toke = stokens(itoke)
    end if
    inumbr = inumbr+1
  end if
!
  stat = .true.
  return
!
end subroutine recvars
!
!>    @brief search for parentheses.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param[in,out] itoke number of tokens
!>    @param iope number of operations
!>    @param iadsb number of addition or subtractions
!>    @param imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param oads addition and subtraction vector
!>    @param omls mult. and division vector
!>    @param oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
recursive&
& subroutine brackets(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
&mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!
  logical :: lstat
  character(len=1) :: br
  character(len=35) :: cms
!
  dimension br(2),cms(2)
!
  parameter(br = (/'(',')'/),&
  &cms = (/'brackets:no matching parentheses   ',&
  &'brackets:oper vector out of bound  '/))
!
  intrinsic :: trim
!
  stat = .false.
!
  if (trim(toke) .eq. br(1)) then
    itoke = itoke+1
!       check bound
    if (itoke .gt. mxstokens) then
      cmsg = cms(2)
      return
    end if
    toke = stokens(itoke)
!       recursive call
    call addsub(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
    if (trim(toke) .ne. br(2)) then
      cmsg = cms(1)
      return
    end if
    if (itoke .lt. ntokens) then
      itoke = itoke+1
!         check bound
      if (itoke .gt. mxstokens) then
        cmsg = cms(2)
        return
      end if
      toke = stokens(itoke)
    end if
    if (trim(toke) .eq. br(1)) then
      cmsg = cms(1)
      return
    end if
  else
    call recvars (nn,ntokens,inumbr,itoke,iope,&
    &mxvarnms,mxstokens,numbr,toke,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
  end if
!
  stat = .true.
  return
!
end subroutine brackets
!
!>    @brief Process parentheses.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param[in,out] itoke number of tokens
!>    @param iope number of operations
!>    @param iadsb number of addition or subtractions
!>    @param imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param oads addition and subtraction vector
!>    @param omls mult. and division vector
!>    @param oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @return true if backets are balanced.
!
recursive&
& logical function prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
&mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
!
  logical :: lstat
  character(len=35) :: cms
  dimension cms(2)
!
  parameter(&
  &cms = (/'prbrak:stokens vec out of bound    ',&
  &'prbrak:oper vector out of bound    '/)&
  &)
!
  prbrak = .false.
!
  itoke = itoke+1
!     check bound
  if (itoke .gt. mxstokens) then
    cmsg = cms(1)
    return
  end if
  toke = stokens(itoke)
  call brackets(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
  &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
  &oads,omls,oper,stokens,varnms,cmsg,lstat)
  if (.not. lstat) return
!     check bound
  if (iope .gt. mxstokens) then
    cmsg = cms(2)
    return
  end if
!
  prbrak = .true.
  return
!
end function prbrak
!
!>    @brief search for known functions.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param itoke number of tokens
!>    @param[in,out] iope number of operations
!>    @param iadsb number of addition or subtractions
!>    @param imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param oads addition and subtraction vector
!>    @param omls mult. and division vector
!>    @param[in,out] oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
recursive&
& subroutine cfunctions (nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
&mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!
  logical :: lstat, prbrak
  character(len=3) :: fn3
  character(len=4) :: fn4
  character(len=5) :: fn5
  character(len=35) :: cms
!
  dimension fn3(6),fn4(9),fn5(3),cms(2)
!
  parameter(&
  &fn3 = (/'sin','cos','tan','log','exp','abs'/),&
  &fn4 = (/'asin','acos','atan','sinh','cosh','tanh',&
  &'nint','aint','sqrt'/),&
  &fn5 = (/'log10','anint','floor'/),&
  &cms = (/'cfunctions:stokens vec out of bound',&
  &'cfunctions:oper vector out of bound'/)&
  &)
!
  intrinsic :: trim
!
  stat = .true.
!
  if (trim(toke) .eq. fn3(1)) then
!       sin
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 8
    iope = iope+1
!
  else if(trim(toke) .eq. fn3(2)) then
!       cos
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 9
    iope = iope+1
!
  else if(trim(toke) .eq. fn3(3)) then
!       tan
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 10
    iope = iope+1
!
  else if(trim(toke) .eq. fn4(1)) then
!       asin
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    iope = iope+1
!
  else if(trim(toke) .eq. fn4(2)) then
!       acos
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 12
    iope = iope+1
!
  else if(trim(toke) .eq. fn4(3)) then
!       atan
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 13
    iope = iope+1
!
  else if(trim(toke) .eq. fn4(4)) then
!       sinh
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 14
    iope = iope+1
!
  else if(trim(toke) .eq. fn4(5)) then
!       cosh
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 15
    iope = iope+1
!
  else if(trim(toke) .eq. fn4(6)) then
!       tanh
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 16
    iope = iope+1
!
  else if (trim(toke) .eq. fn3(4)) then
!       log
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 20
    iope = iope+1
!
  else if (trim(toke) .eq. fn5(1)) then
!       log10
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 21
    iope = iope+1
!
  else if (trim(toke) .eq. fn4(7)) then
!       nint
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 22
    iope = iope+1
!
  else if (trim(toke) .eq. fn5(2)) then
!       anint
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 23
    iope = iope+1
!
  else if (trim(toke) .eq. fn4(8)) then
!       aint
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 24
    iope = iope+1
!
  else if (trim(toke) .eq. fn3(5)) then
!       exp
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 25
    iope = iope+1
!
  else if (trim(toke) .eq. fn4(9)) then
!       sqrt
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 26
    iope = iope+1
!
  else if (trim(toke) .eq. fn3(6)) then
!       abs
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 27
    iope = iope+1
!
  else if (trim(toke) .eq. fn5(3)) then
!       floor
    lstat = prbrak(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg)
    if (.not. lstat) return
    oper(iope) = 28
    iope = iope+1
!
  else
    call brackets(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
  end if
!
  stat = .true.
  return
!
end subroutine cfunctions
!
!>    @brief search for power operations.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param[in,out] itoke number of tokens
!>    @param iope number of operations
!>    @param iadsb number of addition or subtractions
!>    @param imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param oads addition and subtraction vector
!>    @param omls mult. and division vector
!>    @param[in,out] oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
recursive&
& subroutine power (nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
&mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!
  logical :: lstat
  character(len=1) :: op
  character(len=35) :: cms
!
  dimension cms(2)
!
  parameter(op = '^',&
  &cms = (/'power:stokens vector out of bound  ',&
  &'power:oper vector out of bound     '/))
!
  intrinsic :: trim
!
  stat = .false.
!
  call cfunctions(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
  &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
  &oads,omls,oper,stokens,varnms,cmsg,lstat)
  if (.not. lstat) return
  if (trim(toke) .eq. op) then
    itoke = itoke+1
!       check bound
    if (itoke .gt. mxstokens) then
      cmsg = cms(1)
      return
    end if
    toke = stokens(itoke)
    call cfunctions (nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
!       check bound
    if (iope .gt. mxstokens) then
      cmsg = cms(2)
      return
    end if
    oper(iope) = 7
    iope = iope+1
  end if
!
  stat = .true.
  return
!
end subroutine power
!
!>    @brief seach for unary operations.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param[in,out] itoke number of tokens
!>    @param iope number of operations
!>    @param iadsb number of addition or subtractions
!>    @param imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param oads addition and subtraction vector
!>    @param omls mult. and division vector
!>    @param[in,out] oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
recursive&
& subroutine unary(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
&mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!
  logical :: lstat
  character(len=1) :: op
  character(len=35) :: cms
  dimension op(2),cms(2)
!
  parameter(op = (/'+','-'/),&
  &cms = (/'unary:stokens vector out of bound  ',&
  &'unary:oper vector out of bound     '/))
!
  intrinsic :: trim
!
  stat = .false.
!
  if (trim(toke) .eq. op(2)) then
    itoke = itoke + 1
!       check bound
    if (itoke .gt. mxstokens) then
      cmsg = cms(1)
      return
    end if
    toke = stokens(itoke)
    call power(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
!       check bound
    if (iope .gt. mxstokens) then
      cmsg = cms(2)
      return
    end if
    oper(iope) = 2
    iope = iope+1
  else if (trim(toke) .eq. op(1)) then
    itoke = itoke+1
!       check bound
    if (itoke .gt. mxstokens) then
      cmsg = cms(1)
      return
    end if
    toke = stokens(itoke)
    call power(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
  else
    call power(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
  end if
!
  stat = .true.
  return
!
end subroutine unary
!
!>    @brief search for multiplication and division operations.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param[in,out] itoke number of tokens
!>    @param[in,out] iope number of operations
!>    @param iadsb number of addition or subtractions
!>    @param imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[out] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param oads addition and subtraction vector
!>    @param omls mult. and division vector
!>    @param[in,out] oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
recursive&
& subroutine muldiv (nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
&mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!
  logical :: lstat
  character(len=1) :: op
  character(len=35) :: cms
  dimension op(2),cms(2)
!
  parameter(op = (/'*','/'/),&
  &cms = (/'muldiv:stokens vector out of bound ',&
  &'muldiv:oper vector out of bound    '/))
!
  intrinsic :: trim
!
  stat = .false.
!
  call unary(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
  &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
  &oads,omls,oper,stokens,varnms,cmsg,lstat)
  if (.not. lstat) return
!
  do while (trim(toke) .eq. op(1) .or. trim(toke) .eq. op(2))
!
    omls(imldv) = trim(toke)
    imldv = imldv+1
    itoke = itoke+1
!       check bound
    if (itoke .gt. mxstokens) then
      cmsg = cms(1)
      return
    end if
    toke = stokens(itoke)
!
    call unary(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
!
    select case(omls(imldv-1))
     case(op(1))
      imldv = imldv-1
!           check bound
      if (iope .gt. mxstokens) then
        cmsg = cms(2)
        return
      end if
      oper(iope) = 5
      iope = iope+1
     case(op(2))
      imldv = imldv-1
!           check bound
      if (iope .gt. mxstokens) then
        cmsg = cms(2)
        return
      end if
      oper(iope) = 6
      iope = iope+1
    end select
!
  end do
!
  stat = .true.
  return
!
end subroutine muldiv
!
!>    @brief parses addition and subtraction operations.
!
!>    @param nn number of variables in function
!>    @param ntokens number of effective tokens depends on complexity
!>    @param inumbr number of numbers
!>    @param[in,out] itoke number of tokens
!>    @param[in,out] iope number of operations
!>    @param[out] iadsb number of addition or subtractions
!>    @param imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param numbr vector of numbers
!>    @param toke current toke
!>    @param oads addition and subtraction vector
!>    @param omls mult. and division vector
!>    @param[in,out] oper operations vector
!>    @param stokens tokens vector
!>    @param varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
recursive&
& subroutine addsub (nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
&mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!
  logical :: lstat
  character(len=1) :: op
  character(len=35) :: cms
  dimension op(2),cms(3)
!
  parameter(op = (/'+','-'/),&
  &cms = (/'addsub:oads vector out of bound    ',&
  &'addsub:stokens vector out of bound ',&
  &'addsub:oper vector out of bound    '/))
!
  intrinsic :: len
!
  stat = .false.
!
  call muldiv (nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
  &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
  &oads,omls,oper,stokens,varnms,cmsg,lstat)
  if (.not. lstat) return
!
  do while (trim(toke) .eq. op(1) .or. trim(toke) .eq. op(2))
!       check bound
    if (iadsb .gt. mxoads) then
      cmsg = cms(1)
      return
    end if
    oads(iadsb) = trim(toke)
    iadsb = iadsb+1
    itoke = itoke+1
!       check bound
    if (itoke .gt. mxstokens) then
      cmsg = cms(2)
      return
    end if
    toke = stokens(itoke)
!
    call muldiv (nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
    &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
    &oads,omls,oper,stokens,varnms,cmsg,lstat)
    if (.not. lstat) return
!
    select case(oads(iadsb-1))
     case(op(1))
      iadsb = iadsb-1
      oper(iope) = 3
      iope = iope+1
     case(op(2))
      iadsb = iadsb-1
!           check bound
      if (iope .gt. mxstokens) then
        cmsg = cms(3)
        return
      end if
      oper(iope) = 4
      iope = iope+1
    end select
!
  end do
!
  stat = .true.
  return
!
end subroutine addsub
!
!>    @brief scans cfunc storing basic elements.
!
!>    @param[in] cfunc function definition text
!>    @param[in] nn number of variables in function
!>    @param[out] ntokens number of effective tokens depends on complexi
!>    @param[in] inumbr number of numbers
!>    @param[in] itoke number of tokens
!>    @param[in] iope number of operations
!>    @param[in] iadsb number of addition or subtractions
!>    @param[in] imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param[in] numbr vector of numbers
!>    @param[in,out] toke current toke
!>    @param[in] oads addition and subtraction vector
!>    @param[in] omls mult. and division vector
!>    @param[in] oper operations vector
!>    @param[out] stokens tokens vector
!>    @param[in] varnms vector of internal variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
subroutine tokanl (cfunc,nn,ntokens,inumbr,itoke,iope,&
&iadsb,imldv,mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
&oads,omls,oper,stokens,varnms,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nn, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: cfunc, toke, oads, omls, stokens, varnms, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens)
  logical :: stat
!
  character(len=1) :: nb
  character(len=7) :: op
  character(len=11) :: numbers
  character(len=26) :: chars
  character(len=35) :: cms
  dimension nb(5),cms(2)
!
  parameter(&
  &nb = (/' ','e','d','(',')'/),&
  &op = '+-*/^()',&
  &numbers = '.0123456789',&
  &chars = 'abcdefghijklmnopqrstuvwxyz',&
  &cms = (/'tokanl:stokens vector out of bound ',&
  &'tokanl:unbalanced parentheses      '/)&
  &)
!
  integer :: i, k, lc, ln, lf, m
  integer :: irightbrackets, ileftbrackets
  logical :: status, lstat
!
  intrinsic :: index, len_trim, trim
!
  stat = .false.
  ntokens = 0
  irightbrackets = 1
  ileftbrackets = 1
!
  lf = len_trim(cfunc)
!
  k = 1
  i = 1
  do while (k .le. lf)
!       variable, or cfunction name
    lc = index(chars,cfunc(k:k))
    ln = index(numbers,cfunc(k:k))
    if (lc .ne. 0) then
      status = .true.
      do while (status)
        m = index(op, cfunc(k+1:k+1))
        status = m .eq. 0 .and. cfunc(k+1:k+1) .ne. nb(1)
        k = k+1
      end do
      ntokens = ntokens+1
!         number
    else if (ln .ne. 0) then
      status = .true.
      do while (status)
        m = index(op, cfunc(k+1:k+1))
        if ((m .eq. 0 .and. cfunc(k+1:k+1) .ne. nb(1)) .or.&
        &cfunc(k+1:k+1) .eq. nb(2) .or.&
        &cfunc(k+1:k+1) .eq. nb(3)) then
          k = k+1
        else
          if(cfunc(k:k) .eq. nb(2) .or. cfunc(k:k) .eq. nb(3)) then
            k = k+1
          else
            status = .false.
            k = k+1
          end if
        end if
      end do
      ntokens = ntokens+1
!         operator or delimitator
    else
      k = k + 1
      ntokens = ntokens+1
    end if
  end do
!
!     check stokens dimension
  if (ntokens .gt. mxstokens) then
    cmsg = cms(1)
    return
  end if
!
  k = 1
  i = 1
  do while (k .le. lf)
!       variable, or cfunction  name
    lc = index(chars,cfunc(k:k))
    ln = index(numbers,cfunc(k:k))
    if (lc .ne. 0) then
      stokens(i) = cfunc(k:k)
      status = .true.
      do while (status)
        m = index(op, cfunc(k+1:k+1))
        if (m .eq. 0 .and. cfunc(k+1:k+1) .ne. nb(1)) then
          stokens(i) = trim(stokens(i)) // cfunc(k+1:k+1)
          k = k+1
        else
          status = .false.
          k = k+1
          i = i+1
        end if
      end do
!         number
    else if (ln .ne. 0) then
      stokens(i) = cfunc(k:k)
      status = .true.
      do while (status)
        m = index(op, cfunc(k+1:k+1))
        if ((m .eq. 0 .and.&
        &cfunc(k+1:k+1) .ne. nb(1)) .or.&
        &cfunc(k+1:k+1) .eq. nb(2) .or.&
        &cfunc(k+1:k+1) .eq. nb(3)) then
          stokens(i) = trim(stokens(i)) // cfunc(k+1:k+1)
          k = k+1
        else
          if(cfunc(k:k) .eq. nb(2) .or. cfunc(k:k) .eq. nb(3)) then
            stokens(i) = trim(stokens(i)) // cfunc(k+1:k+1)
            k = k+1
          else
            status = .false.
            i = i+1
            k = k+1
          end if
        end if
      end do
!         operator or delimitator
    else
      stokens(i) = cfunc(k:k)
      if(stokens(i) .eq. nb(4)) then
        irightbrackets = irightbrackets+1
      else if(stokens(i) .eq. nb(5)) then
        ileftbrackets = ileftbrackets+1
      end if
      i = i+1
      k = k+1
    end if
  end do
!
  if (irightbrackets .ne. ileftbrackets) then
    cmsg = cms(2)
    return
  end if
!
  itoke = 1
  iadsb = 1
  imldv = 1
  iope = 1
  inumbr = 1
  toke = stokens(itoke)
!
  call addsub(nn,ntokens,inumbr,itoke,iope,iadsb,imldv,&
  &mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
  &oads,omls,oper,stokens,varnms,cmsg,lstat)
  if (.not. lstat) return
!
  iope = iope-1
  stat = .true.
!
  return
!
end subroutine tokanl
!
!>    @brief recognizes variables and set values
!
!>    @param[in,out] cfunc function definition text
!>    @param[in] nvar number of variables in function
!>    @param[out] ntokens number of effective tokens depends on complexi
!>    @param[out] inumbr number of numbers
!>    @param[out] itoke number of tokens
!>    @param[out] iope number of operations
!>    @param[out] iadsb number of addition or subtractions
!>    @param[out] imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param[in] numbr vector of numbers
!>    @param[in] toke current toke
!>    @param[in] oads addition and subtraction vector
!>    @param[in] omls mult. and division vector
!>    @param[in] oper operations vector
!>    @param[out] stokens tokens vector
!>    @param[out] varnms vector of internal variable names
!>    @param[in] varnames vector of given variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
subroutine recvar (cfunc,nvar,ntokens,inumbr,itoke,iope,&
&iadsb,imldv,mxoads,mxomls,mxvarnms,mxstokens,&
&numbr,toke,oads,omls,oper,stokens,varnms,&
&varnames,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nvar, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: cfunc, toke, oads, omls, stokens, varnms, varnames, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens),&
  &varnames(mxvarnms)
  logical :: stat
!
  integer :: ii
  logical :: lstat
!
  stat = .false.
!
  do ii = 1,nvar
    varnms(ii) = varnames(ii)
  end do
!
  call tokanl(cfunc,nvar,ntokens,inumbr,itoke,iope,&
  &iadsb,imldv,mxoads,mxomls,mxvarnms,mxstokens,numbr,toke,&
  &oads,omls,oper,stokens,varnms,cmsg,lstat)
  if (.not. lstat) return
!
  stat = .true.
  return
!
end subroutine recvar
!
!>    @brief removes unnecessary blank spaces
!
!>    @param[in,out] cfunc function definition text
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
subroutine rmblk(cfunc,cmsg,stat)
  implicit none
!
  character(len=*) :: cfunc, cmsg
  logical :: stat
!
  integer :: ii, k, imx
  character(len=1) :: blank
  character(len=35) :: cms
!
  parameter (imx = 1000,blank = ' ',&
  &cms = 'rmblk:it. limit removing blanks    ')
!
  intrinsic :: index, trim
!
  stat = .false.
!
  k = 0
  ii = index(cfunc,blank)
  do while (ii .gt. 0)
    k = k+1
    if (k .gt. imx) then
      cmsg = cms
      return
    end if
    cfunc = cfunc(:ii-1) // cfunc(ii+1:)
    ii = index(trim(cfunc),blank)
  end do
!
  stat = .true.
  return
!
end subroutine rmblk
!
!>    @brief converts pi to number, ln to log, log to log10 and ** to ^
!
!>    @param[in,out] cfunc function definition text
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
subroutine convertb(cfunc,cmsg,stat)
  implicit none
!
  character(len=*) :: cfunc, cmsg
  logical :: stat
!
  character(len=1) :: rb, sb, br, ht, dt
  character(len=2) :: ds, cl, cp
  character(len=3) :: cg
  character(len=10) :: ci
  character(len=35) :: cms
!
  dimension rb(2),sb(2),br(2),dt(2),cl(4),cp(4)
!
  parameter (rb = (/'(',')'/),sb = (/'[',']'/),br = (/'{','}'/),&
  &ht='^',ds = '**',dt = (/',','.'/),cl = (/'ln','Ln','lN','LN'/),&
  &cg = 'log',cp = (/'pi','Pi','pI','PI'/),ci = '3.14159265',&
  &cms = 'convertb:it.limit replacing ** by ^')
!
  integer :: i, k, item, ilength, imx
  parameter (imx = 1000)
!
  intrinsic :: index, len, trim
!
  stat = .false.
!     changes [] and {} by ()
  ilength = len(trim(cfunc))
  do k = 1,ilength
    if(cfunc(k:k) .eq. sb(1) .or.&
    &cfunc(k:k) .eq. br(1)) cfunc(k:k) = rb(1)
    if(cfunc(k:k) .eq. sb(2) .or.&
    &cfunc(k:k) .eq. br(2)) cfunc(k:k) = rb(2)
  end do
  item = 1
  do while(item .eq. 1)
    item = 0
    ilength = len(trim(cfunc))
    if(ilength .gt. 1) then
      do k = 1,ilength-1
!           converts from ^ to **
        if(cfunc(k:k) .eq. ht) then
          cfunc = cfunc(1:k-1)//ds//cfunc(k+1:ilength)
          ilength = ilength+1
          item = 1
        end if
!           converts from ln to log
        if(cfunc(k:k+1) .eq. cl(1) .or.&
        &cfunc(k:k+1) .eq. cl(2) .or.&
        &cfunc(k:k+1) .eq. cl(3) .or.&
        &cfunc(k:k+1) .eq. cl(4)) then
          cfunc = cfunc(1:k-1)//cg//cfunc(k+2:ilength)
          ilength = ilength+1
          item = 1
        end if
!           converts pi em 3.14159
        if(cfunc(k:k+1) .eq. cp(1) .or.&
        &cfunc(k:k+1) .eq. cp(2) .or.&
        &cfunc(k:k+1) .eq. cp(3) .or.&
        &cfunc(k:k+1) .eq. cp(4)) then
          cfunc = cfunc(1:k-1)//ci//cfunc(k+2:ilength)
          ilength = ilength+len(ci)-2
          item = 1
        end if
!           converts comma to dot
        if(cfunc(k:k) .eq. dt(1)) then
          cfunc = cfunc(1:k-1)//dt(2)//cfunc(k+1:ilength)
          item = 1
        end if
      end do
    end if
    if(ilength .gt. 2) then
!         before last
      if(cfunc((ilength-1):(ilength-1)) .eq. ht) then
        cfunc = cfunc(1:ilength-2)//ds//cfunc(ilength:ilength)
        item = 1
      end if
!         before last
      if(cfunc((ilength-1):(ilength-1)) .eq. dt(1)) then
        cfunc = cfunc(1:ilength-2)//dt(2)//cfunc(ilength:ilength)
        item = 1
      end if
    end if
    if(ilength .gt. 1) then
!         last
      if(cfunc((ilength):(ilength)) .eq. dt(1)) then
        cfunc = cfunc(1:ilength-1)//dt(2)
        item = 1
      end if
    end if
  end do
!     converts ** to ^
  i = 0
  k = index(cfunc,ds)
  do while (k .gt. 0)
    i = i+1
!       iteration limit
    if (i .gt. imx) then
      cmsg = cms
      return
    end if
    cfunc = cfunc(:k-1) // ht // cfunc(k+2:)
    k = index(cfunc,ds)
  end do
!
  stat = .true.
  return
!
end subroutine convertb
!
!>    @brief check for common mistakes on given function
!
!>    @param[in,out] cfunc function definition text
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
subroutine identify(cfunc,cmsg,stat)
  implicit none
!
  character(len=*) :: cfunc, cmsg
  logical :: stat
!
  character(len=1) :: cop, cfn, cbl, cbr, cai
  character(len=2) :: cdp, c10
  character(len=3) :: cf31, cf32, cf33
  character(len=4) :: cf4
  character(len=5) :: cer, cf5
  character(len=26) :: upper
  character(len=35) :: cms
  character(len=36) :: variav
!
  dimension cop(5),cfn(8),cdp(15),cf31(3),&
  &cf32(2),cf4(9),cf5(3),cms(7)
!
  parameter(cbl = ' ',cbr = '(',cai = 'h',&
  &cop = (/'-','+','/','*',')'/),&
  &cfn = (/'0','n','s','h','g','t','p','r'/),&
  &c10 = '10',&
  &cdp = (/'--','-+','-/','-*','+-','++','+/','+*',&
  &'*-','*+','*/','/-','/+','//','/*'/),&
  &cf31 = (/'sin','cos','tan'/),&
  &cf32 = (/'abs','exp'/),&
  &cf33 = 'log',&
  &cf4 = (/'asin','acos','atan','sinh','cosh',&
  &'tanh','nint','aint','sqrt'/),&
  &cf5 = (/'log10','anint','floor'/),&
  &cer = 'error',&
  &upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',&
  &variav = '0123456789abcdefghijklmnopqrstuvwxyz',&
  &cms = (/'identify:unfinished oper. at end  ',&
  &'identify:unfinished oper. at start',&
  &'identify:doubled operation signals',&
  &'identify:end parentheses w.o. oper',&
  &'identify:parentheses no known func',&
  &'identify:unknown function         ',&
  &'identify:no function (blank)      '/)&
  &)
!
  integer :: i, j, k, nchar
  logical :: ok

  intrinsic :: len, trim
!
!     init status
  stat = .false.
!
!     length
  nchar = len(trim(cfunc))
  if (nchar .lt. 1) then
    cmsg = cms(7)
    return
  end if
!
!     to lower case
  do i = 1,nchar
    do j = 1,26
      if(cfunc(i:i) .eq. upper(j:j)) then
        k = j+10
        cfunc(i:i) = variav(k:k)
        exit
      end if
    end do
  end do
!
!     unterminated operation at end
  do i = 1,4
    if(cfunc(nchar:nchar) .eq. cop(i)) then
      cmsg = cms(1)
      return
    end if
  end do
!
!     unterminated operation at start
  if(cfunc(1:1) .eq. cop(3) .or. cfunc(1:1) .eq. cop(4)) then
    cmsg = cms(2)
    return
  end if
!
!     double operation signal
  do i = 1,nchar-1
    do j = 1,15
      if(cfunc(i:i+1) .eq. cdp(j))then
        cmsg = cms(3)
        return
      end if
    end do
  end do
!
!     end parentheses without operation
  do i = 1,nchar-1
    do j = 1,36
      if(cfunc(i:i+1) .eq. cop(5)//variav(j:j)) then
        cmsg = cms(4)
        return
      end if
    end do
  end do
!
!     begin parentheses no function
  do i = 1,nchar-1
    do j = 1,36
      ok = .false.
      do k = 1,8
        if(cfunc(i:i) .eq. cfn(k)) then
!             no test, definite cfunction
          ok = .true.
          exit
        end if
      end do
      if (.not. ok) then
        if(cfunc(i:i+1) .eq. variav(j:j)//cbr) then
          cmsg = cms(5)
          return
        end if
      end if
    end do
  end do
!
!     check for known functions
  if(nchar .ge. 5) then
    do k = 1,3
      do i = 1,nchar-4
!           log10,anint,floor
        if(cfunc(i:i+4) .eq. cf5(k)) then
          j = i+5
          do while(cfunc(j:j) .eq. cbl)
            if(j .ge. nchar) then
              cmsg = cms(6)
              return
            end if
            j = j+1
          end do
          if(cfunc(j:j) .ne. cbr) then
            cmsg = cms(6)
            return
          end if
        end if
      end do
    end do
  end if
!
!     check for known functions
  if(nchar .ge. 4) then
    do k = 1,9
      do i = 1,nchar-3
!           asin,acos,atan,sinh,cosh,tanh,nint,aint,sqrt
        if(cfunc(i:i+3) .eq. cf4(k)) then
          j = i+4
          do while (cfunc(j:j) .eq. cbl)
            if(j .ge. nchar) then
              cmsg = cms(6)
              return
            end if
            j = j+1
          end do
          if(cfunc(j:j) .ne. cbr) then
            cmsg = cms(6)
            return
          end if
        end if
      end do
    end do
  end if
!
!     check for known functions
  if(nchar .ge. 3) then
    do k = 1,3
      do i = 1,nchar-2
!           sin,cos,tan
        if(cfunc(i:i+2) .eq. cf31(k)) then
          j = i+3
          do while(cfunc(j:j) .eq. cbl)
            if(j .ge. nchar) then
              cmsg = cms(6)
              return
            end if
            j = j+1
          end do
          if(cfunc(j:j) .ne. cai .and.&
          &cfunc(j:j) .ne. cbr) then
            cmsg = cms(6)
            return
          end if
        end if
      end do
    end do
!
!       abs or exp
    do k = 1,2
      do i = 1,nchar-2
        if(cfunc(i:i+2) .eq. cf32(k)) then
          j = i+3
          do while(cfunc(j:j) .eq. cbl)
            if(j .ge. nchar) then
              cmsg = cms(6)
              return
            end if
            j = j+1
          end do
          if(cfunc(j:j) .ne. cbr) then
            cmsg = cms(6)
            return
          end if
        end if
      end do
    end do
!
!       log
    do i = 1,nchar-2
      if(cfunc(i:i+2) .eq. cf33) then
        j = i+3
        do while(cfunc(j:j) .eq. cbl)
          if(j .ge. nchar) then
            cmsg = cms(6)
            return
          end if
          j = j+1
        end do
        ok = j .lt. (nchar-1) .and. cfunc(j:j+1) .eq. c10
        if(.not. ok .and. cfunc(j:j) .ne. cbr) then
          cmsg = cms(6)
          return
        end if
      end if
    end do
  end if
!
  stat = .true.
  return
!
end subroutine identify
!
!>    @brief Prepares given function for evaluation.
!>     Suggested dimension settings mxoads = 2,mxomls = 2,
!>     mxvarnms = 10,mxstokens = 160.
!
!>    @param[in] cfunc function definition text
!>    @param[in] nvar number of variables in function
!>    @param[in] ntokens number of effective tokens depends on complexit
!>    @param[in] inumbr number of numbers
!>    @param[in] itoke number of tokens
!>    @param[in] iope number of operations
!>    @param[in] iadsb number of addition or subtractions
!>    @param[in] imldv number of multiplication or divisions
!>    @param[in] mxoads dimension of addition and subtraction vector
!>    @param[in] mxomls dimension of mult and division vector
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param[in] numbr vector of numbers
!>    @param[in] toke current toke
!>    @param[in] oads addition and subtraction vector
!>    @param[in] omls mult. and division vector
!>    @param[in] oper operations vector
!>    @param[in] stokens tokens vector
!>    @param[in] varnms vector of internal variable names
!>    @param[in] varnames vector of given variable names
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!
subroutine initev (cfunc,nvar,ntokens,inumbr,itoke,iope,&
&iadsb,imldv,mxoads,mxomls,mxvarnms,mxstokens,&
&numbr,toke,oads,omls,oper,stokens,varnms,&
&varnames,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: numbr
  integer :: nvar, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=*) :: cfunc, toke, oads, omls, stokens, varnms, varnames, cmsg
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens),&
  &varnames(mxvarnms)
  logical :: stat
!
  logical :: lstat
!
  stat = .false.
!     detects errors
  call identify(cfunc,cmsg,lstat)
  if (.not. lstat) return
!     converts pi to number, ln to log, log to log10 and ** to ^
  call convertb(cfunc,cmsg,lstat)
  if (.not. lstat) return
!     remove blanks
  call rmblk(cfunc,cmsg,lstat)
  if (.not. lstat) return
!     recognizes variables and set values
  call recvar (cfunc,nvar,ntokens,inumbr,itoke,iope,&
  &iadsb,imldv,mxoads,mxomls,mxvarnms,mxstokens,&
  &numbr,toke,oads,omls,oper,stokens,varnms,&
  &varnames,cmsg,lstat)
  if (.not. lstat) return
!
  stat = .true.
  return
!
end subroutine initev
!
!>    @brief check indices
!
!>    @param[in] st data index
!>    @param[in] dt number index
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @return true if indices are on bounds
!
logical function chkidx(st,dt,mxstokens)
  implicit none
!
  integer :: st, dt, mxstokens
!
  chkidx = st .gt. 0 .and. st .le. mxstokens .and.&
  &dt .le. mxstokens
!
  return
!
end function chkidx
!
!>    @brief evaluate the expression supplied
!
!>    @param[in] iope number of operations
!>    @param[in] mxvarnms dimension of variables vector
!>    @param[in] mxstokens dimension of stokens, numbers, operation and
!>     evaluation data vector
!>    @param[in] vrls vector of variable values
!>    @param[in] numbr vector of numbers
!>    @param[in] oper operations vector
!>    @param[out] cmsg error message text, valid if stat flag is set
!>    @param[out] stat status of successful completion
!>    @return numeric value of the given expression.
!
real(lrk) function evaluate (iope,mxvarnms,mxstokens,vrls,numbr,oper,cmsg,stat)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iope, mxvarnms, mxstokens, oper
  real(lrk) :: vrls, numbr
  character(len=*) :: cmsg
  dimension vrls(mxvarnms),numbr(mxstokens),oper(mxstokens)
  logical :: stat
!
  real(lrk) :: pdat, d1
  logical :: chkidx
  dimension pdat(mxstokens)
  integer :: st, dt, i
  character(len=4) :: cfn
  character(len=30) :: cms
!
  dimension cfn(3),cms(2)
!
  parameter (d1 = 1,cfn = (/'sind','cosd','tand'/),&
  &cms = (/'evaluate:invalid function:    ',&
  &'evaluate:unhand. var./op./data' /))
!
  intrinsic :: abs, acos, aint, anint, asin, atan, cos, cosh, floor, log, log10, nint, sqrt, sin, sinh, tan, tanh
!
  stat = .false.
  evaluate = 0
  st = 0
  dt = 1
!
  do i = 1, iope
    select case(oper(i))
     case (1)
      st = st+1
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = numbr(dt)
      dt = dt+1
     case (2)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = -d1*pdat(st)
     case (3)
      if (.not. chkidx(st-1,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st-1) = pdat(st-1)+pdat(st)
      st = st-1
     case (4)
      if (.not. chkidx(st-1,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st-1) = pdat(st-1)-pdat(st)
      st = st-1
     case (5)
      if (.not. chkidx(st-1,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st-1) = pdat(st-1)*pdat(st)
      st = st-1
     case (6)
      if (.not. chkidx(st-1,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st-1) = pdat(st-1)/pdat(st)
      st = st-1
     case (7)
      if (.not. chkidx(st-1,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st-1) = pdat(st-1)**pdat(st)
      st = st-1
     case (8)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = sin(pdat(st))
     case (9)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = cos(pdat(st))
     case (10)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = tan(pdat(st))
     case (11)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = asin(pdat(st))
     case (12)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = acos(pdat(st))
     case (13)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = atan(pdat(st))
     case (14)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = sinh(pdat(st))
     case (15)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = cosh(pdat(st))
     case (16)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = tanh(pdat(st))
     case (17)
      cmsg = cms(1)//cfn(1)
      return
     case (18)
      cmsg = cms(1)//cfn(2)
      return
     case (19)
      cmsg = cms(1)//cfn(3)
      return
     case (20)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = log(pdat(st))
     case (21)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = log10(pdat(st))
     case (22)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = nint(pdat(st))
     case (23)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = anint(pdat(st))
     case (24)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = aint(pdat(st))
     case (25)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = exp(pdat(st))
     case (26)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = sqrt(pdat(st))
     case (27)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = abs(pdat(st))
     case (28)
      if (.not. chkidx(st,dt,mxstokens)) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = floor(pdat(st))
     case default
      st = st+1
      if (.not. chkidx(st,dt,mxstokens) .or.&
      &oper(i)-28 .lt. 1 .or. oper(i)-28 .gt. mxstokens) then
        cmsg = cms(2)
        return
      end if
      pdat(st) = vrls(oper(i)-28)
    end select
  end do
!
  stat = .true.
  evaluate = pdat(1)
!
  return
!
end function evaluate
!
!>    @brief generate time and torque values for a given
!>     torque time function on transient torsion.
!>    torque function must use "t" as only dependent variable.
!
!>    @param[in] tid transient torque index
!>    @param[in] std torque multiplier
!>    @param[in] ttrdsz number of time points
!>    @param[in] mxttrd dimension time torque transients vector
!>    @param[in] mxttra dimendion time and value torque data
!>    @param[in] ttrdtt time data vector
!>    @param[out] ttrdta torque data matrix
!>    @param[in] cfunc torque time function, must have only "t" as
!>      time dependent variable.
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tgntp(tid,std,ttrdsz,mxttrd,mxttra,&
&ttrdtt,ttrdta,cfunc,errmsg,ok)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk
  implicit none
!
  character(len=99) :: errmsg
  character(len=255) :: cfunc
  integer :: tid, ttrdsz, mxttrd, mxttra, ok
  real(lrk) :: std, ttrdtt, ttrdta
  dimension ttrdtt(mxttrd),ttrdta(mxttra,mxttrd)
!
!     common variables
  real(lrk) :: numbr
  integer :: nvar, ntokens, inumbr, itoke, iope, iadsb, imldv, oper, mxoads, mxomls, mxvarnms, mxstokens
  character(len=10) :: varnms, varnames
  character(len=255) :: toke, stokens, oads, omls
!     set dimension
  parameter(mxoads = 2,mxomls = 2,&
  &mxvarnms = 1,mxstokens = 160)
!
  dimension numbr(mxstokens),oads(mxoads),omls(mxomls),&
  &oper(mxstokens),varnms(mxvarnms),stokens(mxstokens),&
  &varnames(mxvarnms)
!
  integer :: ii
!     variable values, used on evaluator
  real(lrk) :: vrls, evaluate, ans
  logical :: stat
  character(len=1) :: ctv
  character(len=5) :: nmm
  character(len=35) :: cmsg
  dimension vrls(mxvarnms)
  parameter(ctv = 't',nmm = 'tgntp')
!     init variable names and values
  data varnames/mxvarnms*' '/,vrls/mxvarnms*0/
!
  ok = -1
!
!     number of variables
  nvar = 1
!     variable t for time
  varnames(1) = ctv
!     init evaluator
  call initev (cfunc,nvar,ntokens,inumbr,itoke,iope,&
  &iadsb,imldv,mxoads,mxomls,mxvarnms,mxstokens,&
  &numbr,toke,oads,omls,oper,stokens,varnms,&
  &varnames,cmsg,stat)
  if (.not. stat) then
    errmsg = fomsgf(99, nmm,16,cmsg,0)
    return
  end if
!
!     evaluate for each time
  do ii = 1,ttrdsz
!       set time
    vrls(1) = ttrdtt(ii)
    ans = evaluate (iope,mxvarnms,mxstokens,vrls,numbr,oper&
    &,cmsg,stat)
    if (.not. stat) then
      errmsg = fomsgf(99, nmm,16,cmsg,0)
      return
    end if
    ttrdta(tid,ii) = std*ans
  end do
!
  ok = 0
  return
!
end subroutine tgntp
!
