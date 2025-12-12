program TEST6

  
  CHARACTER(LEN=200) :: w
  integer :: sy
    



10 call system('clear')
 call system('date +"%Y-%m-%d %H:%M:%S"')
 print *, "    "
 print *, "                                                                                           [‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾]"
 print *, "                                                                                           [Type lst for list of programs.]"
 print *, "                                                                                           [______________________________]"
 print *, "                                                                                           "      
 print *, "    "
do
 call suba() 
 write(*,'(A)',advance='no') '->'
  read (*, '(A)') w

  if (w == 'wrt') then
        call tone()
 	call sub()
  elseif (w == 'end') then
 	exit
  elseif (w == 'lst') then
        call tone()
 	call suba()
  elseif (w == 'clc') then
        call tone()
 	call add()
  elseif (w == 'tax') then
        call tone()
 	call tax()
  elseif (w == 'art') then
        call tone()
        call art()
  elseif (w == 'bsh') then
        call tone()
 	call bash()
  elseif (w == 'pnt') then
        call tone()
 	call point()
  elseif (w == 'gmb') then
        call tone()
        call gamble()
  elseif (w == 'qud') then
        call tone()
        call seconddeg()
  elseif (w == 'yeq') then
        call tone()
        call yeq()
  elseif (w == 'tri') then
        call tone()
        call tri()  
  elseif (w == 'rld') then
        call tone()
        goto 10
elseif (w == 'clr') then
        call tone()
 	call system('clear')
elseif (w == 'stop') then
        call tone()
        call system('shutdown -h  now')
else
       print *, "                                                                                                          <?>"
       call tone() 
 end if
 end do




contains




subroutine tone()

    call system('echo -n "Loading" | minimodem --tx --ascii 200')

end subroutine tone



subroutine sub()
 CHARACTER(LEN=200) :: z
 print *, "------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
 print *, "                                                                                                  Write 'stop' to quit."
 print *, "------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
 do
  read (*, '(A)') z
  if (z == 'stop') then
        call tone()
 	exit
  end if
 end do
end subroutine sub



subroutine gamble()
real :: x
real :: y

do
read (*,*) x
if (x == y) then
    call tone()
    exit
end if
write(*, '(F1.0)') y
end do
end subroutine gamble



subroutine suba()
   print *, "                                                                                              __________________________"
   print *, "                                                                                              [List of current programs]"
   print *, "                                                                                              [------------------------]"
   print *, "                                                                                              [-> wrt................<-]"
   print *, "                                                                                              [-> lst................<-]"
   print *, "                                                                                              [-> clc................<-]"
   print *, "                                                                                              [-> tax................<-]"
   print *, "                                                                                              [-> pnt................<-]"
   print *, "                                                                                              [-> gmb................<-]"
   print *, "                                                                                              [-> rld................<-]"
   print *, "                                                                                              [-> tri................<-]"
   print *, "                                                                                              [-> bsh................<-]"
   print *, "                                                                                              [-> qud................<-]"
   print *, "                                                                                              [-> yeq................<-]"
   print *, "                                                                                              [-> clr................<-]"
   print *, "                                                                                              [-> stop...............<-]"
   print *, "                                                                                              ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾"
   print *, "                           "
end subroutine suba



subroutine add()

  real :: y
  real :: x
  CHARACTER(LEN=200) :: h
  print *, "                                                                                                _____________________________"
  print *, "                                                                                               [Type '99, stop,  99' to quit.]"
  print *, "                                                                                                ￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣"
 do
  read(*,*) x, h, y
  if (h == 'stop') then
 	exit
 elseif (x == 99) then
 	exit
 elseif (y == 99) then
        call tone()
 	exit
 elseif (h == '+') then
 	print *, " ", x+y
 elseif (h == '-') then
 	print *, " ", x-y
 elseif (h == 'x') then
 	print *, " ", x*y
 elseif (h == '/') then
 	print *, " ", x/y
 elseif (h == 'p') then
 	print *, " ", x**2+y**2
 elseif (h == 't') then
 	print *, " ", x*x+y*y
 elseif (h == 'h') then
 	print *, " ", x*x-y*y
 elseif (h == 'sin') then
 	print *, " ", sin(x/y)
 elseif (h == 'cos') then
 	print *, " ", cos(x/y)
 elseif (h == 'tan') then
 	print *, " ", tan(x/y)

  end if
 end do
end subroutine add



subroutine tax()

real :: ta
real :: tr
tr = 0.13

 do
print *, "                                                                                                 ______________________________  "
print *, "                                                                                                [To quit enter the number '999'] " 
print *, "                                                                                                 ￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣  "
read(*,*) ta
 if (ta == 999) then
        call tone()
 	exit
 end if
 	print *, ta*tr
end do
end subroutine tax



subroutine point()

real :: p !x in eq1
real :: w !y in eq2
real :: q !x in eq2
real :: t !y in eq1
real :: x1 ! x1-x2
real :: y1 ! y1-y2
real :: m ! v/c
real :: g
real :: x
real :: k
g=x/c
 do
  print *, "                                                                                   ______________________________________________________"
  print *, "                                                                                  [Put in 2 equasions to find the points of intersection.]"
  print *, "                                                                                  [Input 999, 0 to quit..................................]"
  print *, "                                                                                   ￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣"
  read(*,*) p, t
 if (p == 999) then
   call tone() 
   exit
 end if
  read(*,*) q, w
  x1=p-q
  c =t-w
  m=c/x1
  ! print *, v
  ! print *, c
  print *, 'x = ', m
  k=p*m+t
  print *, 'y = ', k
  print *, "   "
end do
end subroutine point



subroutine bash()
integer :: sy
CHARACTER(LEN=200) :: h
CHARACTER(LEN=200) :: c
CHARACTER(LEN=200) :: p
call chdir(trim(adjustl(p)))

 print *, "                                                                                                    __________________"
 print *, "                                                                                                   [Type 'stop to quit]"
 print *, "                                                                                                    ￣￣￣￣￣￣￣￣￣"
 do
  write(*,'(A)',advance='no') 'BASH->'
  read(*,*) h
 	c = trim(adjustl(h))
 call execute_command_line(c, exitstat=sy)
if (h == 'stop') then
   call tone()
   exit
end if
end do
end subroutine bash



subroutine yeq()
real :: x1
real :: x2
real :: y1
real :: y2
real :: b
real :: m

do
print *, "To quit input 999, 0"
 read(*,*) x1, y1
if (x1 == 999) then
 call tone()
 exit
end if
 read(*,*) x2, y2
 m=(y2-y1)/(x2-x1)
 print *, m
 b=-m*x1+y1
 print *, "y=", m, "x+", b
 print *, "   "
end do
end subroutine yeq



subroutine seconddeg()
real :: a
real :: b
real :: c
real :: d
real :: x1
real :: x2

do
print *, "Input a, b and c, to exit, input 999, 0, 0"
read(*,*) a, b, c
d = b*b - 4.0*a*c

if (d < 0.0) then
	print *, "not possible"
elseif (a == 999) then
 call tone()
 exit
else
	x1 = (-b + sqrt(d)) / (2.0*a)
	x2 = (-b - sqrt(d)) / (2.0*a)
	print *, " ", x1, x2
    end if
  end do
end subroutine seconddeg




subroutine tri()

real :: a
real :: b
real :: c
real :: ta
real :: tg
real :: th
real :: h
do

read(*,*) a
read(*,*) b
read(*,*) c

if (a>c) then
 print *, "                                                                                                         impossible!"
elseif (b>c) then
 print *, "                                                                                                         impossible!"
end if


 ta = ASIN(a/c)
 tg = ACOS(b/c)
 th = ATAN(a/b)

print *, " '1' is sin, '2' is cos, '3' is tan"

read(*,*) h
if (h == 1) then
print *, "                                                                                                         angle a is", ta
elseif (h == 2) then
print *, "                                                                                                         angle b is", tg
elseif (h == 3) then
print *, "                                                                                                         angle c is", th 
end if

end do
end subroutine tri  

subroutine art()

print *, "     .         .       =#=                                .                    .                   ."
print *, "                    .*#####+.                         .      .                                      "
print *, "               .  .##########*.         .  . .                                                      "
print *, "                .*#############+.       .   .  .                                             .      "
print *, "               =#################*.                 ........                                        "
print *, "             .######################:  .     .-**##############**-.    ..   .                       "
print *, "            =########################-.  .=*#########################+.                      .  .   "
print *, "          .+######################=.   .=###############################*:           .   .          "
print *, "          +####################*:.    . ..=################################-.                .     ."
print *, "        .*####################:.            -#######++====+*#################- ..:::::::..          "
print *, "        *###################=.             . .::.             .:=*###########################+-.    "
print *, " .     +###################.                                  ..-+*#############################*:  "
print *, "      -##################*.                               :=*#####################-:...      .:-+#+."
print *, "     :##################*.                       . . .-+###############**##########.              .:"
print *, "     +#################+.      ..                .-*#############*+-.    =##########.               "
print *, "    :##################.      .      ..      .:+#############+:           :#########*           .   "
print *, "   .*#################:.                  .-*###########*=:.   .           =#########=.             "
print *, "   .#################*..             . .=############+:                 .   +#########.             "
print *, "   -#################-              .+###########*-.                        -#########-             "
print *, "  .=################*.     .    ..=###########+.                            .#########+       .     "
print *, "  .*################+         .-###########=.                               .#########*.            "
print *, "  .#################=   .   :*##########=.                                  .#########*.            "
print *, "  .#################-.   .-##########=.       .                    .        .#########*.           ."
print *, "  .#################=  .*#########*.                                        :#########+             "
print *, "  .*################+.#########*:               .                           +#########=      .    . "
print *, "   =#########################+.        .                                   :##########:             "
print *, "   -#######################:.     .             .                         .*#########*.             "
print *, "   .#####################:.                                 .             *##########.              "
print *, "   ..*##################:                                    .            +##########=.             " 
print *, "    :##################.         .                                     .###########=                "
print *, "     +#################+                                             .=###########=.        .    .  "
print *, "     :##################*.                                 .       .:############-                  "
print *, "     .-##################*.                                      .=############*.                   "
print *, "     .*####################:                          .        .=#############-          .          "
print *, "    :#######################+.                        .          .+#########=                       "
print *, "   -##########################=.     .                             .=#####-.                      . "
print *, "  -######:*#####################+.                       .           .==.                     .     "
print *, " :#####+  .+#######################-.                           .         .     .                   "
print *, ".#####=    .=########################*=.                .   .        .:+##-.                  .     "
print *, "-####=       :###########################*=:.         .          .:=########=.                      "
print *, "*###=          =################################+=-:::::::--=+################=.     .             ."
print *, "####.           .*##############################################################+.                  "
print *, "=##-   .          :###############################################################+.                "
print *, ".#*.                :*##############################################################*..             "
print *, " :*                   .+###########################################################*:               "
print *, "   ..                   .-######################################################*:                  "
print *, ".  .+*:..  .         ...:-+##################################################=.                    ."
print *, "     :###################################################################+.                         "
print *, "        -#####################*:.  .:*##############################*:.                            ."
print *, "              .::--::..      .  .         :=+*##############*=-.       .                            "

end subroutine art

end program TEST6

