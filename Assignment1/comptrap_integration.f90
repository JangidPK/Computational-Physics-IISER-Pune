!     -------------------------------------------------------------------------
  module param
    !     -------------------------------------------------------------------------
        implicit none
        real*8, parameter :: zero = 0.00000d+00, half = 0.500000d+00
        real*8, parameter :: one  = 1.00000d+00, two = 2.0000000d+00
        real*8, parameter :: pi = 2 * asin(one)
      end
    !     -------------------------------------------------------------------------
    
      program wigner_distribution
        use param
        implicit none
        real*8 :: a, b  ! limits of integral
        real*8 :: fun, pannel_area, t_area,  h, x_i, x_j 
        integer*4 :: i,  n  ! number of pannel in the range 
        write(*,*) "Enter the number of pannel to be used : "
        read(*,*) n
        a = zero
        b = one
        h = (b - a)/(n* one)
        pannel_area = zero
        t_area = zero
        
        do i = 1, n 
         x_i = a + (i-1)*h 
         x_j = a + i * h

         pannel_area = (fun(x_i) + fun(x_j)) * h / two 

         t_area  = t_area + pannel_area
        enddo 
!-----------------------------------------------------------------------------        
        write(*,*) "Area calculated from composite trapezoidal : "
        write(*,*) t_area
        write(*,*) "Value of Pi "
        write(*,*) pi
        write(*,*) "Error in integral calculation : "
        write(*,*) pi - t_area
          
      end program wigner_distribution
 !---------------------------------------------------------------------------   
    	function fun(x)
    	  use param
    		implicit none
    		real*8 :: x, fun    		
    		fun = one * 4 * (one / (one + x**2)) 
    	end function
