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
    use momenta, only:
    use strains, only:
    implicit none

    type material_parameters
        real(real12) :: row_spacing
    end type

    integer, parameter :: X_BARRIER = 1, Y_BARRIER = 2

contains


pure real(real12) function Gamma_k(k_3D, row_direction, material_params)
    type(material_parameters), intent(in) :: material_params
    real(real12), intent(in) :: k_3D(3)
    integer, intent(in) :: row_direction

    real(real12) :: k_dir(3), prefactor
    integer :: m_pmax, m_mmin
    real(real12), allocatable :: unity(:)
    integer, allocatable :: m_values(:)

    if (row_direction == 1) then
        k_dir = k_3D
    else if (row_direction == 2) then
        k_dir = [k_3D(2), k_3D(1), k_3D(3)]
    else 
        ! signal failure somehow
    end if 

    m_pmax = m_max(k_dir(1:2), material_params%row_spacing)
    m_mmin = m_min(k_dir(1:2), material_params%row_spacing)
    allocate(unity(m_mmin:m_pmax), m_values(m_mmin: m_pmax))
    unity = one
    m_values = arange(m_mmin, m_pmax, increment=1)

    prefactor = one

    Gamma_k = prefactor*sum(element(k_dir(1)*unity, k_dir(2)*unity, k_dir(3)*unity, material_params%row_spacing*unity, m_values))

contains

    pure integer function m_max(k_2D, row_spacing)
        real(real12), intent(in) :: k_2D(2), row_spacing

        m_max = max(floor(row_spacing/two*pi + sqrt(k_2D(1)*k_2D(1)+k_2D(2)*k_2D(2))),0)
    end function

    pure integer function m_min(k_2D, row_spacing)
        real(real12), intent(in) :: k_2D(2), row_spacing

        m_min = min(ceiling(row_spacing/two*pi - sqrt(k_2D(1)*k_2D(1)+k_2D(2)*k_2D(2))),0)
    end function

    elemental real function element(k_1,k_2, k_3, row_spacing, m_value)
        real(real12), intent(in) :: k_1, k_2, k_3, row_spacing
        integer, intent(in) :: m_value
    end function

end function



end module scattering_rate