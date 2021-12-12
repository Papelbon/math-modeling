module simulation_record

    use server
    use event
    use customer
    use csv_file
    use ogpf

    implicit none

    private

    type(gpf) :: gp

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    type, private :: tevent_record

        character(len=10) :: event_type

        real(wp) :: current_simulation_time

        integer :: current_queue_size

        integer :: current_busy_servers

        integer :: customer

        integer :: server

    contains

        private

        procedure, pass :: init => event_record_init

    end type tevent_record

    type, private :: tcustomer_record

        real(wp) :: arrival_time

        real(wp) :: service_start_time

        real(wp) :: departure_time

        integer ::  server

        integer :: id

        real(wp) :: service_time;

        real(wp) :: queue_delay_time;

    contains

        private

        procedure, pass :: init => customer_record_init

    end type tcustomer_record

    type, public :: tsimulation_record

        type(tcustomer_record), allocatable, private :: customer_records(:)
        type(tevent_record), allocatable, private :: event_records(:)
        real(wp), allocatable, private :: state_durations(:)
        real(wp), allocatable, private :: state_probabilities(:)
        real(wp), allocatable, private :: event_state_timepoint_durations(:,:)

        integer, private :: total_servers
        integer, private :: total_customers
        integer, private :: total_states
        integer, private :: customer_records_len
        integer, private :: event_records_len

        real(wp), private :: total_simulation_time
        real(wp), private :: total_queue_length
        real(wp), private :: total_queue_delay_time
        real(wp), private :: total_service_time
        real(wp), private :: total_arrival_time

        real(wp), private :: average_arrival_time
        real(wp), private :: average_queue_length
        real(wp), private :: average_queue_delay_time
        real(wp), private :: average_service_time

        real(wp), private :: server_utilization

    contains

        private

        procedure, pass, private :: create_summary

        procedure, pass, public :: init => simulation_record_init
        procedure, pass, public :: create_event_record
        procedure, pass, public :: create_customer_record
        procedure, pass, public :: log_event_records
        procedure, pass, public :: log_customer_records
        procedure, pass, public :: create_stats
        procedure, pass, public :: show_stats

    end type tsimulation_record

contains

    subroutine simulation_record_init(this, total_servers, total_customers, total_states)

        implicit none

        class(tsimulation_record):: this
        integer, intent(in) :: total_servers
        integer, intent(in) :: total_customers
        integer, intent(in) :: total_states

        this%total_servers = total_servers
        this%total_states = total_states
        this%total_customers = total_customers

        this%total_simulation_time = 0
        this%total_queue_length = 0
        this%total_queue_delay_time = 0
        this%total_service_time = 0
        this%total_arrival_time = 0

        this%average_arrival_time = 0
        this%average_queue_length = 0
        this%average_queue_delay_time = 0
        this%average_service_time = 0
        this%server_utilization = 0

        this%customer_records_len = 0
        this%event_records_len = 0

        allocate(this%customer_records(total_customers))
        allocate(this%event_records(total_customers*4))
        if (total_states > 0) then
            allocate(this%state_durations(total_states))
            this%state_durations = 0._wp
        end if

    end subroutine simulation_record_init

    subroutine customer_record_init(this, customer)

        implicit none

        class(tcustomer_record):: this
        type(tcustomer), intent(in) :: customer

        this%id = customer%get_id()
        this%arrival_time = customer%get_arrival_time()
        this%departure_time = customer%get_departure_time()
        this%service_start_time = -1
        this%server = -1
        this%queue_delay_time = this%departure_time - this%arrival_time
        this%service_time = -1

        if (customer%get_served()) then
            this%service_start_time = customer%get_service_start_time()
            this%server = customer%get_server()
            this%queue_delay_time = this%service_start_time - this%arrival_time
            this%service_time = this%departure_time - this%service_start_time
        end if

    end subroutine customer_record_init

    subroutine event_record_init(this, event_type, server_id, current_simulation_time, customer_id, current_queue_size, &
            current_busy_servers)

        implicit none

        class(tevent_record):: this
        character(len=*), intent(in) :: event_type
        real(wp), intent(in) :: current_simulation_time
        integer, intent(in) :: server_id, customer_id, current_queue_size, current_busy_servers

        this%event_type = event_type
        this%server = server_id
        this%current_simulation_time = current_simulation_time
        this%customer = customer_id
        this%current_queue_size = current_queue_size
        this%current_busy_servers = current_busy_servers

    end subroutine event_record_init

    subroutine create_event_record(this, event_type, server_id, simulation_time, customer_id, queue_size, busy_servers)

        implicit none

        class(tsimulation_record):: this
        character(len=*), intent(in) :: event_type
        real(wp), intent(in) :: simulation_time
        integer, intent(in) :: server_id, customer_id, queue_size, busy_servers

        this%event_records_len = this%event_records_len + 1
        call this%event_records(this%event_records_len)%init(event_type, server_id, simulation_time, &
                customer_id, queue_size, busy_servers)

    end subroutine create_event_record

    subroutine create_customer_record(this, customer)

        implicit none

        class(tsimulation_record):: this
        type(tcustomer), intent(in) :: customer

        this%customer_records_len = this%customer_records_len + 1
        call this%customer_records(this%customer_records_len)%init(customer)

    end subroutine create_customer_record

    subroutine create_summary(this)

        implicit none

        class(tsimulation_record):: this
        integer :: i, j, state_idx, busy_servers, queue_size
        real(wp) :: time_gap

        this%total_simulation_time = this%customer_records(this%customer_records_len)%departure_time

        do i = 1, this%total_customers
            if (i == 1) then
                time_gap = this%customer_records(i)%arrival_time
            else
                time_gap = this%customer_records(i)%arrival_time - this%customer_records(i-1)%arrival_time
            end if
            this%total_arrival_time = this%total_arrival_time + time_gap
            this%total_queue_delay_time = this%total_queue_delay_time + this%customer_records(i)%queue_delay_time
            if (.not. this%customer_records(i)%service_time == -1) then
                this%total_service_time = this%total_service_time + this%customer_records(i)%service_time
            end if
        end do

        if (this%total_states > 0) then
            allocate(this%event_state_timepoint_durations(this%event_records_len-1, this%total_states))
            this%event_state_timepoint_durations = 0._wp
        end if

        do i = 2, this%event_records_len
            time_gap = this%event_records(i)%current_simulation_time - this%event_records(i-1)%current_simulation_time
            queue_size = this%event_records(i-1)%current_queue_size
            this%total_queue_length = this%total_queue_length + time_gap * queue_size

            if (this%total_states > 0) then
                busy_servers = this%event_records(i-1)%current_busy_servers
                state_idx = busy_servers+queue_size+1
                this%state_durations(state_idx) = this%state_durations(state_idx)+time_gap

                do j = 1, this%total_states
                    if (j == state_idx) then
                        this%event_state_timepoint_durations(i-1, j) = this%state_durations(j)/ &
                                this%event_records(i)%current_simulation_time
                    else
                        if (.not.i == 2) then
                            this%event_state_timepoint_durations(i-1, j) = this%event_state_timepoint_durations(i-2, j)
                        end if
                    end if
                end do

            end if
        end do

    end subroutine create_summary

    subroutine create_stats(this)

        implicit none

        class(tsimulation_record):: this

        call this%create_summary()

        this%average_arrival_time = this%total_arrival_time / this%total_customers
        this%average_queue_length = this%total_queue_length / this%total_simulation_time
        this%average_queue_delay_time = this%total_queue_delay_time / this%total_customers
        this%average_service_time = this%total_service_time / this%total_customers
        this%server_utilization = this%total_service_time / this%total_simulation_time
        if (this%total_states > 0) then
            this%state_probabilities = this%state_durations / this%total_simulation_time
        end if
    end subroutine create_stats

    subroutine show_stats(this)

        implicit none

        class(tsimulation_record):: this
        real(wp), allocatable :: x(:), y(:)
        integer :: i

        print '(a16)', "Simulation Stats"

        print '(a30,i15)',   "Total servers:              ", this%total_servers

        print '(a30,i15)',   "Total customers:            ", this%total_customers

        print '(a30,f15.7)', "Total simulation time:      ", this%total_simulation_time

        print '(a30,f15.7)', "Average arrival time:       ", this%average_arrival_time

        print '(a30,f15.7)', "Average service time:       ", this%average_service_time

        print '(a30,f15.7)', "Average queue delay time:   ", this%average_queue_delay_time

        print '(a30,f15.7)', "Average system time:        ", this%average_service_time + this%average_queue_delay_time

        print '(a30,f15.7)', "Average queue length:       ", this%average_queue_length

        print '(a30,f15.7)', "Average customers in system:", this%average_queue_length + this%server_utilization

        print '(a30,f15.7)', "Server utilization:         ", this%server_utilization

        if (this%total_states > 0) then
            allocate(x(this%total_states))
            x = linspace(1._wp, real(this%total_states, wp), this%total_states)
            call gp%title('Simulation probabilities')
            call gp%plot(x, this%state_probabilities, 'with impulses lw 3 lc rgb "#4285f4"')

            print '(a30,f15.7)', "Refuse probability:         ", this%state_probabilities(this%total_states)
            print '(a30,f15.7)', "Relative throughput:        ", 1 - this%state_probabilities(this%total_states)
            print '(a30,f15.7)', "Absolute throughput:        ", &
                    this%average_arrival_time * (1 - this%state_probabilities(this%total_states))

            allocate(y(this%event_records_len-1))
            y = [(this%event_records(i)%current_simulation_time, i=2,this%event_records_len)]
            call gp%title('Stationary mode set-up')
            call gp%plot(y, this%event_state_timepoint_durations, 'w lines')
        end if

    end subroutine show_stats

    subroutine log_event_records(this, filename)

        implicit none

        class(tsimulation_record):: this
        character(len=*), intent(in) :: filename
        character(len=12) :: header(6) = (/ &
                "Event Type  ", "Start Time  ", "Server ID   ", "Customer ID ", "Queue Size  ", "Busy Servers" &
        /)
        integer :: i

        open(unit=1, file=filename, status='unknown')

        call csv_write(1, header, .true.)

        do i = 1, this%event_records_len
            call csv_write(1, this%event_records(i)%event_type, .false.)
            call csv_write(1, this%event_records(i)%current_simulation_time, .false.)
            call csv_write(1, this%event_records(i)%server, .false.)
            call csv_write(1, this%event_records(i)%customer, .false.)
            call csv_write(1, this%event_records(i)%current_queue_size, .false.)
            call csv_write(1, this%event_records(i)%current_busy_servers, .true.)
        end do

        close(1)

    end subroutine log_event_records

    subroutine log_customer_records(this, filename)

        implicit none

        class(tsimulation_record):: this
        character(len=*), intent(in) :: filename
        character(len=18) :: header(7) = (/ &
                "Customer ID       ", "Server ID         ", "Arrival Time      ", "Service Start Time", &
                "Departure Time    ", "Service Duration  ", "Queue Delay Time  " &
        /)
        integer :: i

        open(unit=2, file=filename, status='unknown')

        call csv_write(2, header, .true.)

        do i = 1, this%customer_records_len
            call csv_write(2, this%customer_records(i)%id, .false.)
            call csv_write(2, this%customer_records(i)%server, .false.)
            call csv_write(2, this%customer_records(i)%arrival_time, .false.)
            call csv_write(2, this%customer_records(i)%service_start_time, .false.)
            call csv_write(2, this%customer_records(i)%departure_time, .false.)
            call csv_write(2, this%customer_records(i)%service_time, .false.)
            call csv_write(2, this%customer_records(i)%queue_delay_time, .true.)
        end do

        close(2)

    end subroutine log_customer_records

end module simulation_record
