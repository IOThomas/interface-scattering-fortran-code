!------------------------------------------------------------------------------
! Institution, Affiliation
!------------------------------------------------------------------------------
!
! MODULE:  Module name
!
!> @author
!> Author Name}
!
! DESCRIPTION: 
!>  Short module description
!
! REVISION HISTORY:
! dd Mmm yyyy - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------
module momenta
    use constants, only: real12, pi, zero, one, two
    use, intrinsic :: ieee_arithmetic
    implicit none

contains

    pure real(real12) function omega(group_velocity, momentum_3D_mag)
        real(real12), intent(in) :: group_velocity, momentum_3D_mag

        omega = group_velocity*momentum_3D_mag
    end function

    pure real(real12) function q_m(m, distance)
        integer(real12), intent(in) :: m
        real(real12), intent(in) :: distance

        q_m = two*pi*real(m, real12)/distance
    end function

    pure real(real12) function k1b_msigma(k_1, k_2, q_2m, sigma)
        real(real12), intent(in) :: k_1, k_2, q_2m
        integer, intent(in) :: sigma

        integer :: plusminus
        real(real12) :: root

        plusminus=sign(1, sigma)
        
        root = k_1*k_1 - two*k_2*q_2m -q_2m**2

        if (root >= zero) then 
            k1b_msigma = sqrt(root)*real(plusminus,real12)
        else
            k1b_msigma = ieee_value(k1b_msigma, ieee_signaling_nan)
        end if
    
    end function

    pure real(real12) function q1_msigma(k_1, k_2, q_2m, sigma)
        real(real12), intent(in) :: k_1, k_2, q_2m
        integer, intent(in) :: sigma

        q1_msigma = k1b_msigma(k_1, k_2, q_2m, sigma) - k_1
    end function

    pure real(real12) function k_mag(k_vec)
        real(real12), intent(in) :: k_vec(:)

        k_mag = sqrt(sum(k_vec*k_vec))

    end function

    pure real(real12) function mom_scaling_suppression(k_1, k_2, k_3, q_2m, sigma, group_velocity)
        real(real12), intent(in) :: k_1, k_2, k_3, q_2m, group_velocity
        integer, intent(in) :: sigma
        ! corresponds to omega_k*J(k,m)*(1-k.k') term

        real(real12) :: numerator, denominator, k_3D

        k_3D = k_mag([k_1, k_2, k_3])

        if (k_3D /= zero) then
            ! this is allowing for non-linear forms of omega at some later point
            numerator = (k_2*q_2m - q1_msigma(k_1, k_2, q_2m, sigma))*omega(group_velocity, k_mag([k_1, k_2, k_3]))
            denominator = k_mag([k_1, k_2, k_3])*abs(k1b_msigma(k_1,k_2,q_2m,sigma))
        else
            numerator = (k_2*q_2m - q1_msigma(k_1, k_2, q_2m, sigma))*group_velocity ! assumes acoustic, near zero k_3D
            denominator = abs(k1b_msigma(k_1,k_2,q_2m,sigma))
        end if
        

        mom_scaling_suppression = numerator/denominator

        

    end function



end module