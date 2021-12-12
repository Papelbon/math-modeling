module server

    use customer

    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    integer :: total_servers = 0

    public :: tserver_status, IDLE, BUSY

    enum, bind(c)

        ! Enumeration name
        enumerator :: tserver_status = 0

        enumerator :: IDLE
        enumerator :: BUSY

    end enum

    type, public :: tserver

        integer(kind(tserver_status)), private :: server_status

        integer, private :: id;

        type(tcustomer), private :: current_customer

    contains

        private

        procedure, pass, public :: init

        procedure, pass, public :: get_server_status
        procedure, pass, public :: get_current_customer

        procedure, pass, public :: set_server_status
        procedure, pass, public :: set_current_customer

    end type tserver

contains

    subroutine init(this)

        implicit none

        class(tserver) :: this

        total_servers = total_servers + 1
        this%server_status = IDLE
        this%id = total_servers

    end subroutine init

    subroutine set_server_status(this, server_status)

        implicit none

        class(tserver) :: this
        integer(kind(tserver_status)), intent(in) :: server_status

        this%server_status = server_status

    end subroutine set_server_status

    subroutine set_current_customer(this, current_customer)

        implicit none

        class(tserver) :: this
        type(tcustomer) :: current_customer

        this%current_customer = current_customer

    end subroutine set_current_customer

    integer(kind(tserver_status)) function get_server_status(this)

        implicit none

        class(tserver) :: this

        get_server_status = this%server_status;

    end function get_server_status

    function get_current_customer(this)

        implicit none

        class(tserver) :: this
        type(tcustomer) :: get_current_customer

        get_current_customer = this%current_customer;

    end function get_current_customer

end module server