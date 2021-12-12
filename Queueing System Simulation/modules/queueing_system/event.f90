module event

    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    public :: tevent_type, ARRIVAL, DEPARTURE

    enum, bind(c)

        ! Enumeration name
        enumerator :: tevent_type = 0

        enumerator :: ARRIVAL
        enumerator :: DEPARTURE

    end enum

    type, public :: tevent

        integer(kind(tevent_type)), private :: event_type

        real(wp), private :: invoke_time

        integer, private :: server

        integer, private :: customer

    contains

        private

        procedure, pass, public :: init

        procedure, pass, public :: get_invoke_time
        procedure, pass, public :: get_server
        procedure, pass, public :: get_customer
        procedure, pass, public :: get_type

    end type tevent

contains

    subroutine init(this, event_type, invoke_time, server_id, customer_id)

        implicit none

        class(tevent) :: this
        real(wp), intent(in) :: invoke_time
        integer(kind(tevent_type)), intent(in) :: event_type
        integer, intent(in), optional :: server_id, customer_id

        this%event_type = event_type
        this%invoke_time = invoke_time
        this%server = -1
        if (present(server_id)) this%server = server_id
        this%customer = -1
        if (present(customer_id)) this%customer = customer_id

    end subroutine init

    real(wp) function get_invoke_time(this)

        implicit none

        class(tevent) :: this

        get_invoke_time = this%invoke_time;

    end function get_invoke_time

    integer(kind(tevent_type)) function get_type(this)

        implicit none

        class(tevent) :: this

        get_type = this%event_type;

    end function get_type

    integer function get_server(this)

        implicit none

        class(tevent) :: this

        get_server = this%server;

    end function get_server

    integer function get_customer(this)

        implicit none

        class(tevent) :: this

        get_customer = this%customer;

    end function get_customer

end module event