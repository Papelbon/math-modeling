module customer

    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    integer :: total_customers = 0;

    type, public :: tcustomer

        real(wp), private :: arrival_time

        real(wp), private :: departure_time

        real(wp), private :: service_start_time

        integer, private ::  server

        integer, private :: id

        logical, private :: served

    contains

        private

        procedure, pass, public :: init

        procedure, pass, public :: get_service_start_time
        procedure, pass, public :: get_total_customers
        procedure, pass, public :: get_departure_time
        procedure, pass, public :: get_arrival_time
        procedure, pass, public :: get_id
        procedure, pass, public :: get_server
        procedure, pass, public :: get_served

        procedure, pass, public :: set_service_start_time
        procedure, pass, public :: set_departure_time
        procedure, pass, public :: set_server
        procedure, pass, public :: set_served

    end type tcustomer

contains

    subroutine init(this, arrival_time)

        implicit none

        class(tcustomer):: this
        real(wp), intent(in) :: arrival_time

        total_customers = total_customers + 1
        this%id = total_customers
        this%arrival_time = arrival_time
        this%served = .true.
        this%server = -1

    end subroutine init

    integer function get_total_customers(this)

        implicit none

        class(tcustomer) :: this

        get_total_customers = total_customers;

    end function get_total_customers

    integer function get_id(this)

        implicit none

        class(tcustomer) :: this

        get_id = this%id;

    end function get_id

    subroutine set_service_start_time(this, service_start_time)

        implicit none

        class(tcustomer) :: this
        real(wp), intent(in) :: service_start_time

        this%service_start_time = service_start_time

    end subroutine set_service_start_time

    subroutine set_departure_time(this, departure_time)

        implicit none

        class(tcustomer) :: this
        real(wp), intent(in) :: departure_time

        this%departure_time = departure_time

    end subroutine set_departure_time

    real(wp) function get_arrival_time(this)

        implicit none

        class(tcustomer) :: this

        get_arrival_time = this%arrival_time;

    end function get_arrival_time

    real(wp) function get_service_start_time(this)

        implicit none

        class(tcustomer) :: this

        get_service_start_time = this%service_start_time;

    end function get_service_start_time

    real(wp) function get_departure_time(this)

        implicit none

        class(tcustomer) :: this

        get_departure_time = this%departure_time;

    end function get_departure_time

    subroutine set_server(this, server_id)

        implicit none

        class(tcustomer) :: this
        integer, intent(in) :: server_id

        this%server = server_id

    end subroutine set_server

    integer function get_server(this)

        implicit none

        class(tcustomer) :: this

        get_server = this%server;

    end function get_server

    subroutine set_served(this, is_served)

        implicit none

        class(tcustomer) :: this
        logical, intent(in) :: is_served

        this%served = is_served

    end subroutine set_served

    logical function get_served(this)

        implicit none

        class(tcustomer) :: this

        get_served = this%served;

    end function get_served


end module customer