program namer

    implicit none

    ! Decalre the variables
    
    character :: name*10
    
    ! Ask Me to write my name
    
    print *,"What is your name?"
    
    ! recognize the typing from terminal
    
    read *, name
    
    ! print out my name
    
    print *,"My name is ",name
    
    
    end program namer