module customer_queue

    use customer

    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    type, public :: tcustomer_queue

        type(tcustomer), allocatable, private :: buf(:)

        integer, private :: n = 0

    contains

        private

        procedure, pass, public :: pop
        procedure, pass, public :: push
        procedure, pass, public :: empty
        procedure, pass, public :: len
        procedure, pass, public :: find
        procedure, pass, public :: remove

    end type tcustomer_queue

contains

    function pop(this) result (customer)

        implicit none

        class(tcustomer_queue) :: this

        type(tcustomer) :: customer

        if (this%n > 0) then
            customer = this%buf(1)
            this%n = this%n - 1
            if (this%n > 0) then
                this%buf(1:this%n) = this%buf(2:this%n+1)
            end if
        else
            stop "tcustomer_queue%pop() - queue is empty"
        end if

    end function pop

    integer function find(this, customer_id)

        implicit none

        class(tcustomer_queue) :: this
        integer, intent(in) :: customer_id
        integer :: i

        find = 0
        do i = 1, this%n
            if (this%buf(i)%get_id() == customer_id) then
                find = i
                return
            end if
        end do

    end function find

    function remove(this, customer_id) result (customer)

        implicit none

        class(tcustomer_queue) :: this

        type(tcustomer) :: customer
        integer, intent(in) :: customer_id
        integer :: loc

        if (this%n > 0) then
            loc = this%find(customer_id)
            if (loc > 0) then
                customer = this%buf(loc)
                this%n = this%n - 1
                if (this%n > 0) then
                    this%buf(loc:this%n) = this%buf(loc+1:this%n+1)
                end if
            end if
        else
            stop "tcustomer_queue%remove() - queue is empty"
        end if

    end function remove

    logical function empty(this)

        class(tcustomer_queue) :: this

        empty = this%n < 1

    end function empty

    integer function len(this)

        class(tcustomer_queue) :: this

        len = this%n

    end function len

    subroutine push(this, customer)

        class(tcustomer_queue), intent(inout) :: this

        type(tcustomer) :: customer
        type(tcustomer), allocatable :: tmp(:)

        this%n = this%n + 1
        if (.not.allocated(this%buf)) allocate(this%buf(1))
        if (size(this%buf) < this%n) then
            allocate(tmp(2*size(this%buf)))
            tmp(1:this%n-1) = this%buf
            call move_alloc(tmp, this%buf)
        end if
        this%buf(this%n) = customer

    end subroutine push

end module customer_queue