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
module scattering_rate
    use functional
    use constants, only: real12, pi, two, one
    use momenta, only: q1_msigma, q_m, mom_scaling_suppression
    use strains, only: E_Delta2, E_R2, E_S2
    use, intrinsic :: ieee_arithmetic
    implicit none

    type material_parameters
        real(real12) :: row_spacing
        real(real12) :: group_velocity
        real(real12) :: nu
        real(real12) :: burgess_vector
    end type

    integer, parameter :: X_BARRIER = 1, Y_BARRIER = 2
    integer, parameter :: NEGATIVE = -1, FLAT = 0, POSITIVE = 1

contains


pure real(real12) function Gamma_k(k_3D, row_direction, material_params)
    type(material_parameters), intent(in) :: material_params
    real(real12), intent(in) :: k_3D(3)
    integer, intent(in) :: row_direction

    real(real12) :: k_dir(3), prefactor, not_a_number
    integer :: m_pmax, m_mmin
    real(real12), allocatable :: unity(:)
    integer, allocatable :: m_values(:)

    if (row_direction == 2) then
        k_dir = k_3D
    else if (row_direction == 1) then
        k_dir = [k_3D(2), k_3D(1), k_3D(3)]
    else 
        not_a_number = ieee_value(Gamma_k, ieee_signaling_nan)
        k_dir = [not_a_number, not_a_number, not_a_number]
    end if 

    associate(row_spacing => material_params%row_spacing, group_velocity => material_params%group_velocity, nu => material_params%nu, burgers_vector => material_params%burgess_vector)

        m_pmax = m_max(k_dir(1:2), row_spacing)
        m_mmin = m_min(k_dir(1:2), row_spacing)
        allocate(unity(m_mmin:m_pmax), m_values(m_mmin: m_pmax))
        unity = one
        m_values = arange(m_mmin, m_pmax, increment=1)

        prefactor = one

        Gamma_k = prefactor*sum(element(k_dir(1)*unity, k_dir(2)*unity, k_dir(3)*unity, row_direction*int(unity), row_spacing*unity, group_velocity*unity, nu*unity, burgers_vector*unity, m_values))

    end associate

    end function

    pure integer function derivative_sign(k_2D, row_spacing, m)
        real(real12), intent(in) :: k_2D(2), row_spacing, m

        real(real12) :: derivative, mom_space
        

        mom_space = two*pi/row_spacing

        derivative = two*mom_space*(k_2D(2) - mom_space*m)

        if (derivative > 0) then 
            derivative_sign = POSITIVE
        else if (derivative < 0) then 
            derivative_sign = NEGATIVE
        else
            derivative_sign = FLAT
        end if

    end function

    pure integer function m_max(k_2D, row_spacing)
        real(real12), intent(in) :: k_2D(2), row_spacing

        real(real12) :: m
        integer :: slope

        m = (k_2D(2) + sqrt(k_2D(1)*k_2D(1)+k_2D(2)*k_2D(2)))*row_spacing/(two*pi)

        slope = derivative_sign(k_2D, row_spacing, m)

        if (slope == POSITIVE) then
            m_max = max(ceiling(m),0)
        elseif (slope == NEGATIVE) then
            m_max = max(floor(m),0)
        elseif (slope == FLAT) then
            m_max = max(nint(m),0)
        end if

    end function

    pure integer function m_min(k_2D, row_spacing)
        real(real12), intent(in) :: k_2D(2), row_spacing

        real(real12) :: m
        integer :: slope

        m = (k_2D(2) + sqrt(k_2D(1)*k_2D(1)+k_2D(2)*k_2D(2)))*row_spacing/(two*pi)

        slope = derivative_sign(k_2D, row_spacing, m)

        if (slope == POSITIVE) then
            m_min = max(ceiling(m),0)
        elseif (slope == NEGATIVE) then
            m_min = max(floor(m),0)
        elseif (slope == FLAT) then
            m_min = max(nint(m),0)
        end if
    end function

    elemental real(real12) function element(k_1,k_2, k_3, row_dir, row_spacing, group_velocity, nu, burgers_vector, m_value, sigma)
        real(real12), intent(in) :: k_1, k_2, k_3, row_spacing, group_velocity
        real(real12), intent(in) :: nu, burgers_vector
        integer, intent(in) :: m_value, row_dir, sigma

        element = potential(q1_msigma(k_1, k_2, q_m(m_value,row_spacing),sigma), q_m(m_value, row_spacing)) * mom_scaling_suppression(k_1, k_2, k_3, q_m(m_value, row_spacing), sigma, group_velocity)

        contains

            pure real(real12) function potential(q_1, q_2)
                real(real12), intent(in) :: q_1, q_2
            
                real(real12) :: q_x, q_y

            ! if row-direction = 1, need to swap back the momenta for the potentials
                if (row_dir==1) then
                    q_y = q_1 
                    q_x = q_2
                else 
                    q_x = q_1 
                    q_y = q_2
                endif

                potential = E_Delta2(nu, burgers_vector, q_x, q_y) + E_R2(nu, burgers_vector, q_x, q_y) + E_S2(nu, burgers_vector, q_x, q_y)


            end function

    end function



end module scattering_rate