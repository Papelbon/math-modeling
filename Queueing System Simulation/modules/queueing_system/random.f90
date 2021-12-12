module random

    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    type, public :: trand

        real(wp), private :: mean
        integer, private :: order

    contains

        private

        procedure, pass, public :: init
        procedure, pass, public :: erlang
        procedure, pass, public :: get_mean
        procedure, pass, public :: get_order

    end type trand

contains

    subroutine init(this, mean, order)

        implicit none

        class(trand) :: this
        real(wp), intent(in) :: mean
        integer, intent(in) :: order

        this%mean = mean
        this%order = order

    end subroutine init

    real(wp) function erlang(this)

        implicit none

        class(trand):: this
        integer :: i
        real(wp) :: r

        erlang = 0._wp
        do i = 1, this%order ! order=1 => exponential
            call random_number(r)
            erlang = erlang - this%mean * log(r) / this%order
        end do

    end

    real(wp) function get_mean(this)

        implicit none

        class(trand):: this

        get_mean = this%mean

    end

    integer function get_order(this)

        implicit none

        class(trand):: this

        get_order = this%order

    end

end module random