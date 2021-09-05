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
        integer*4 :: i, counter,   n   ! number of pannel in the range 
        
        n = 1 
        counter = 0 

        open(10, file = 'errordata.txt')

       do while ( n < 1000000000)
        counter = counter + 1
         
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
     
        write(10,"(*(f22.16))")log(n*one),  log(abs(pi - t_area))
        n =  5**(counter)
       
       enddo 

      end program 
 !--------------------------------------------------------------------------- 
 			
 
   
    	function fun(x)
    	  use param
    		implicit none
    		real*8 :: x, fun    		
    		fun = one * 4 * (one / (one + x**2)) 
    	end function
