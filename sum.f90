program sum

    implicit none
    
    !Declare Variables
    
    real :: x,y,answer
    
    print *, 'Enter 2 Numbers'
    
    read *, x 
    read *, y
    
    answer = x+y
    
    print *, 'The total is ', answer
    
    end program sum 