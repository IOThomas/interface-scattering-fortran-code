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
module strains
    use constants, only: real12, pi, zero, one, two
    implicit none

contains

    pure real(real12) function metric(a,b)
        real(real12), intent(in) :: a, b

        metric = sqrt(a**2 + b**2)
    end function

    pure real(real12) function E_Delta2(nu, burgers_vector, q_x, q_y)
        real(real12), intent(in):: nu, burgers_vector, q_x, q_y

        complex(real12) :: E_Delta
        real(real12) :: numerator, denominator, q

        q = metric(q_x, q_y)
        numerator = (one - two*nu)*q_y
        denominator = (one - nu)*q*q

        E_Delta = cmplx(zero, burgers_vector*numerator/denominator, real12)

        E_Delta2 = real(E_Delta*conjg(E_Delta),real12)

    end function

    pure real(real12) function E_S2(nu, burgers_vector, q_x, q_y)
        real(real12), intent(in):: nu, burgers_vector, q_x, q_y

        complex(real12) :: E_S
        real(real12) :: numerator, denominator, q

        q = metric(q_x, q_y)
        numerator = q_x*q_y*q_y
        denominator = (one - nu)*q*q*q*q

        E_S = cmplx(zero, -burgers_vector*numerator/denominator, real12)
        E_S2 = real(E_S*conjg(E_S), real12)

    end function

    pure real(real12) function E_R2(nu, burgers_vector, q_x, q_y)
        real(real12), intent(in):: nu, burgers_vector, q_x, q_y

        complex(real12) :: E_R
        real(real12) :: numerator, denominator, q

        q = metric(q_x, q_y)
        numerator = q_x
        denominator = q_y

        E_R = cmplx(zero, -two*burgers_vector*numerator/denominator, real12)
        E_R2 = real(E_R*conjg(E_R), real12)

    end function

end module