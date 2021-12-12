module simulation

    use customer
    use customer_queue
    use event
    use event_queue
    use server
    use random
    use simulation_record
    use ogpf

    implicit none

    private

    type(gpf) :: gp

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    type, public :: tsimulation

        real(wp), private :: clock;

        type(tsimulation_record), private :: simulation_record

        type(tserver), allocatable, private :: servers(:)

        type(tcustomer_queue), private :: customer_queue

        integer, private :: max_customer_queue_size
        logical, private :: unlimited_customer_queue = .true.

        type(tevent_queue), private :: event_queue

        type(trand), private :: arrival_time_rgen

        type(trand), private :: service_time_rgen

        type(trand), private :: queue_delay_time_rgen
        logical, private :: unlimited_queue_delay_time = .true.

        integer, private :: total_customers

        integer, private :: total_servers

        integer, private :: total_states

    contains

        private

        procedure, pass, public :: init

        procedure, pass, private :: update_clock
        procedure, pass, private :: get_available_server
        procedure, pass, private :: get_total_busy_servers
        procedure, pass, private :: handle_arrival
        procedure, pass, private :: handle_departure
        procedure, pass, private :: start_service
        procedure, pass, private :: create_departure_record
        procedure, pass, private :: create_service_record
        procedure, pass, private :: create_arrival_record

        procedure, pass, public :: run
        procedure, pass, public :: get_simulation_record
        procedure, pass, public :: show_theoretical_stats

    end type tsimulation

contains

    subroutine init(this, total_servers, total_customers, arrival_time_mean, arrival_time_order, &
            service_time_mean, service_time_order, queue_delay_time_mean, queue_delay_time_order,&
            max_customer_queue_size)

        implicit none

        class(tsimulation):: this
        type(tevent) :: event
        integer :: i

        integer, intent(in) :: total_servers, total_customers
        integer, intent(in), optional :: max_customer_queue_size
        integer, intent(in) :: arrival_time_order, service_time_order
        real(wp), intent(in) :: arrival_time_mean, service_time_mean
        real(wp), intent(in), optional :: queue_delay_time_mean
        integer, intent(in), optional :: queue_delay_time_order

        this%total_servers = total_servers
        this%total_customers = total_customers
        this%total_states = 0
        this%max_customer_queue_size = -1
        if (present(max_customer_queue_size)) this%max_customer_queue_size = max_customer_queue_size
        if (this%max_customer_queue_size >= 0) then
            this%unlimited_customer_queue = .false.
            this%total_states = this%total_servers + this%max_customer_queue_size + 1
        end if
        call this%arrival_time_rgen%init(arrival_time_mean, arrival_time_order)
        call this%service_time_rgen%init(service_time_mean, service_time_order)
        if (present(queue_delay_time_mean)) then
            this%unlimited_queue_delay_time = .false.
            if (present(queue_delay_time_order)) then
                call this%queue_delay_time_rgen%init(queue_delay_time_mean, queue_delay_time_order)
            else
                call this%queue_delay_time_rgen%init(queue_delay_time_mean, 1)
            end if
        end if

        this%clock = 0._wp

        allocate(this%servers(total_servers))
        do i = 1, total_servers
            call this%servers(i)%init()
        end do

        call event%init(ARRIVAL, this%clock + this%arrival_time_rgen%erlang())
        call this%event_queue%push(event)

        call this%simulation_record%init(this%total_servers, this%total_customers, this%total_states)

    end subroutine init

    subroutine update_clock(this, clock)

        implicit none

        class(tsimulation) :: this
        real(wp), intent(in) :: clock

        this%clock = clock

    end subroutine update_clock

    integer function get_available_server(this)

        implicit none

        class(tsimulation) :: this
        integer :: i

        get_available_server = -1

        do i = 1, this%total_servers
            if (this%servers(i)%get_server_status() == IDLE) then
                get_available_server = i
                exit
            end if
        end do


    end function get_available_server

    integer function get_total_busy_servers(this)

        implicit none

        class(tsimulation) :: this
        integer :: i

        get_total_busy_servers = 0

        do i = 1, this%total_servers
            if (this%servers(i)%get_server_status() == BUSY) then
                get_total_busy_servers = get_total_busy_servers + 1
            end if
        end do

    end function get_total_busy_servers

    subroutine start_service(this, customer, server_id)

        implicit none

        class(tsimulation) :: this
        type(tcustomer) :: customer
        integer, intent(in) :: server_id
        type(tevent) :: departure_event

        call customer%set_server(server_id)
        call customer%set_service_start_time(this%clock)
        call this%servers(server_id)%set_current_customer(customer)

        call this%servers(server_id)%set_server_status(BUSY)

        call departure_event%init(DEPARTURE, this%clock + this%service_time_rgen%erlang(), server_id=server_id)
        call this%event_queue%push(departure_event)

        call this%create_service_record(customer)

    end subroutine start_service

    subroutine handle_arrival(this)

        implicit none

        class(tsimulation) :: this
        type(tcustomer) :: new_customer
        type(tevent) :: arrival_event, departure_event
        integer :: server_id

        call new_customer%init(this%clock)

        if (this%total_customers > new_customer%get_total_customers()) then
            call arrival_event%init(ARRIVAL, this%clock + this%arrival_time_rgen%erlang())
            call this%event_queue%push(arrival_event)
        end if

        call this%create_arrival_record(new_customer);

        server_id = this%get_available_server()
        if (server_id == -1) then
            if (this%unlimited_customer_queue .or. this%customer_queue%len() < this%max_customer_queue_size) then
                call this%customer_queue%push(new_customer)
                if (.not.this%unlimited_queue_delay_time) then
                    call departure_event%init(DEPARTURE, this%clock + this%queue_delay_time_rgen%erlang(), &
                            customer_id=new_customer%get_id())
                    call this%event_queue%push(departure_event)
                end if
            else
                call new_customer%set_departure_time(this%clock)
                call new_customer%set_served(.false.)
                call this%create_departure_record(new_customer)
            end if
        else
            call this%start_service(new_customer, server_id)
        end if

    end subroutine handle_arrival

    subroutine handle_departure(this, server_id, customer_id)

        implicit none

        class(tsimulation) :: this
        integer, intent(in) :: server_id, customer_id
        type(tcustomer) :: curr_customer, next_customer
        integer :: loc

        if (customer_id == -1) then
            call this%servers(server_id)%set_server_status(IDLE)
            curr_customer = this%servers(server_id)%get_current_customer()
        else
            loc = this%customer_queue%find(customer_id)
            if (loc == 0) return
            curr_customer = this%customer_queue%remove(customer_id)
            call curr_customer%set_served(.false.)
        end if
        call curr_customer%set_departure_time(this%clock)
        call this%create_departure_record(curr_customer)

        if (server_id == -1) return

        if (.not.this%customer_queue%empty()) then
            next_customer = this%customer_queue%pop()
            call this%start_service(next_customer, server_id)
        end if

    end subroutine handle_departure

    subroutine run(this)

        implicit none

        class(tsimulation) :: this
        type(tevent) :: current_event

        do while(.not.this%event_queue%empty())

            current_event = this%event_queue%pop()
            call this%update_clock(current_event%get_invoke_time())

            if (current_event%get_type() == ARRIVAL) then
                call this%handle_arrival()
            else
                call this%handle_departure(current_event%get_server(), current_event%get_customer())
            end if

        end do

        call this%simulation_record%create_stats()

    end subroutine run

    subroutine create_arrival_record(this, customer)

        implicit none

        class(tsimulation) :: this
        type(tcustomer), intent(in) :: customer

        call this%simulation_record%create_event_record("Arrival", customer%get_server(), customer%get_arrival_time(), &
                customer%get_id(), this%customer_queue%len(), this%get_total_busy_servers())

    end subroutine create_arrival_record

    subroutine create_service_record(this, customer)

        implicit none

        class(tsimulation) :: this
        type(tcustomer), intent(in) :: customer

        call this%simulation_record%create_event_record("Service", customer%get_server(), &
                customer%get_service_start_time(), customer%get_id(), this%customer_queue%len(), &
                this%get_total_busy_servers())

    end subroutine create_service_record

    subroutine create_departure_record(this, customer)

        implicit none

        class(tsimulation) :: this
        type(tcustomer), intent(in) :: customer

        call this%simulation_record%create_event_record("Departure", customer%get_server(), &
                customer%get_departure_time(), customer%get_id(), this%customer_queue%len(), &
                this%get_total_busy_servers())

        call this%simulation_record%create_customer_record(customer)

    end subroutine create_departure_record

    subroutine show_theoretical_stats(this)

        implicit none

        class(tsimulation) :: this
        real(wp) :: ro, arrival_rate, service_rate, queue_delay_rate
        real(wp) :: average_queue_length, average_processing, average_queue_delay_time, average_system_time
        real(wp), allocatable :: probabilities(:), x(:)
        real(wp) :: nsum1, nsum2, nprod
        integer :: arrival_order, service_order, i, k

        arrival_rate = 1._wp / this%arrival_time_rgen%get_mean()
        service_rate = 1._wp / this%service_time_rgen%get_mean()

        arrival_order = this%arrival_time_rgen%get_order()
        service_order = this%service_time_rgen%get_order()

        ro = arrival_rate / service_rate

        if (this%total_servers == 1 .and. &
                this%unlimited_customer_queue .and. this%unlimited_queue_delay_time .and. &
                arrival_order == 1) then

            average_queue_length = ro**2 * (1 + 1._wp/service_order) / (2 - 2*ro)
            average_queue_delay_time = average_queue_length / arrival_rate
            print '(a17)', "Theoretical Stats"
            print '(a30,i15)',   "Total servers:              ", this%total_servers
            print '(a30,f15.7)', "Average queue delay time:   ", average_queue_delay_time
            print '(a30,f15.7)', "Average system time:        ", average_queue_delay_time + 1/service_rate
            print '(a30,f15.7)', "Average queue length:       ", average_queue_length
            print '(a30,f15.7)', "Average customers in system:", ro + average_queue_length

        else if (this%total_servers > 1 .and. &
                .not.this%unlimited_customer_queue .and. .not.this%unlimited_queue_delay_time .and. &
                arrival_order == 1 .and. service_order == 1) then

            queue_delay_rate = 1._wp / this%queue_delay_time_rgen%get_mean()
            allocate(probabilities(this%total_states))
            nsum1 = 0._wp
            do i = 0, this%total_servers
                nsum1 = nsum1 + ro**i / product((/(k,k=1,i)/))
            end do
            nsum2 = 0._wp
            do i = 1, this%max_customer_queue_size
                nprod = 1._wp
                do k = 1, i
                    nprod = nprod * (this%total_servers + k*queue_delay_rate/service_rate)
                end do
                nsum2 = nsum2 + ro**i / nprod
            end do
            probabilities(1) = 1._wp / (nsum1+(ro**this%total_servers/product((/(i,i=1,this%total_servers)/)))*nsum2)

            average_processing = 0._wp
            do i = 1, this%total_servers
                probabilities(i+1) = ro**i * probabilities(1) / product((/(k,k=1,i)/))
                average_processing = average_processing + i * probabilities(i+1)
            end do
            average_queue_length = 0._wp
            do i = 1, this%max_customer_queue_size
                nprod = 1._wp
                do k = 1, i
                    nprod = nprod * (this%total_servers + k*queue_delay_rate/service_rate)
                end do
                probabilities(this%total_servers+i+1) = ro**(this%total_servers+i) * probabilities(1) / &
                        (product((/(k,k=1,this%total_servers)/)) * nprod)
                average_queue_length = average_queue_length + i * probabilities(this%total_servers+i+1)
            end do

            average_queue_delay_time = average_queue_length / arrival_rate
            print '(a17)', "Theoretical Stats"
            print '(a30,i15)',   "Total servers:              ", this%total_servers
            print '(a30,f15.7)', "Average queue delay time:   ", average_queue_delay_time
            print '(a30,f15.7)', "Average system time:        ", average_queue_delay_time + &
                    (1 - probabilities(this%total_states)) / service_rate
            print '(a30,f15.7)', "Average queue length:       ", average_queue_length
            print '(a30,f15.7)', "Average customers in system:", average_processing + average_queue_length

            allocate(x(this%total_states))
            x = linspace(1._wp, real(this%total_states, wp), this%total_states)
            call gp%title('Theoretical probabilities')
            call gp%plot(x, probabilities, 'with impulses lw 3 lc rgb "#4285f4"')

            print '(a30,f15.7)', "Refuse probability:         ", probabilities(this%total_states)
            print '(a30,f15.7)', "Relative throughput:        ", 1 - probabilities(this%total_states)
            print '(a30,f15.7)', "Absolute throughput:        ", &
                    this%arrival_time_rgen%get_mean() * (1 - probabilities(this%total_states))

        else
            print '(a16)', "Not Implemented."
        end if

    end subroutine show_theoretical_stats

    function get_simulation_record(this)

        implicit none

        class(tsimulation) :: this
        type(tsimulation_record) :: get_simulation_record

        get_simulation_record = this%simulation_record

    end function get_simulation_record

end module simulation