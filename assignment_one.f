      program assignment_one
        implicit none

        real velocity, ft_sec, lift_co, calc_lift

        integer alt_input, wingSurfaceArea, weight,
     *  pitch, i, j, k, l, m, n, o, cruise

        integer atk_angles(21)
        real altitude_density(21, 2), coefficient(21),
     *   lifts(21,21)

        cruise = 400
        velocity = ft_sec(cruise)

        wingSurfaceArea = 1140
        weight = 52500


!     Opening and reading from a table of values stored in a text file
        open (unit = 1, file = "Atmos_Table.txt",
     *   form = "FORMATTED", status = "OLD", action = "READ")


!       Ask for user input for the altitude
        write(*,*) "What is the altitude of the Nighthawk? "
        read(*,*) alt_input

!     Iterates through the given table of altitudes and air densities
!     If the user altitude is found in the table then the pitch is set to that index
!     The lift coefficient/attack angle is calculated from the current iter value -1
       do 10 i = 1, 21
         read (UNIT = 1, FMT = *) altitude_density(i, 1),
     *   altitude_density(i, 2)
         if(altitude_density(i,1).eq.alt_input) pitch = i
         coefficient(i) = lift_co(i-1)
         atk_angles(i) = i-1
10     continue
       close (unit = 1)

!     Tries to fill the table with calculated lifts but ends up with zeros ????
       do 1 j = 1,21
        do 2 k = 1,21
            lifts(j,k) = calc_lift(altitude_density(j, 2),
     *      velocity, wingSurfaceArea, coefficient(k))
!      VALUES ARE PRESENT BUT CALCULATION IS NOT DONE????
!            write(*,*) altitude_density(j,2)
!              write(*,*) coefficient(k)
2       continue
1       continue

!     Tries to find the pitch given the user alt
       do 3 o = 1, 21
        if (lifts(pitch, o).ge.weight) pitch = atk_angles(o)
        if (lifts(pitch, o).ge.weight) exit
3      continue


!      Formatting the table to look reasonably readable
      write (*,*) "                         Attack Angle or Pitch Angle"
      write(*,*) ("-----", n=1,78)
      write (*,20) "alt", (atk_angles(m), m=1,21)
20    format(A3,21(I18))

      do 30 l = 1,21
         write (*,40) altitude_density(l,1), (lifts(l,m), m=1,21)
30    continue

40    format(F7.1,21(F18.2))
      write(*,*) ("-----", n=1,78)

      write(*,*) "The required pitch angle to maintain level flight at",
     * alt_input, "ft is", pitch
      stop
      end





      !!!!!!!!!!!!!!!!!!!!FUNCTIONS!!!!!!!!!!!!!!!!!!!!!!!!!!

!       Converts knots into feet per second
      real function ft_sec (num)
        integer num
        ft_sec = num * 6076 / 360
        return
        end

!     Calculates the lift coefficient (provided by NASA)
      real function lift_co (a)
          integer a
          if (a.lt.16) lift_co = (.080625*a) + 0.16
          if (a.ge.16) lift_co = (-.009874372*a*a)+
     *   (.3721357*a-1.974673)
          return
          end

!!     Calculates lift from provided arguments (formula provided)
      real function calc_lift(velo, wingSA, dens, coLift)
!           integer wingSA
           real velo, dens, coLift, wingSA

        calc_lift = (.5 * dens)*(velo*velo)*(wingSA)*(coLift)
        return
        end
