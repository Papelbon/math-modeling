module event_queue

    use event

    implicit none

    private

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: wp = dp

    type, private :: tevent_node

        type(tevent) :: event

        real(wp) :: priority

    end type tevent_node

    type, public :: tevent_queue

        type(tevent_node), allocatable, private :: buf(:)

        integer, private :: n = 0

    contains

        private

        procedure, pass, private :: siftdown

        procedure, pass, public :: push
        procedure, pass, public :: pop
        procedure, pass, public :: empty
        procedure, pass, public :: len

    end type tevent_queue

contains

    subroutine siftdown(this, a)

        class(tevent_queue) :: this

        integer :: a, parent, child

        associate (x => this%buf)

            parent = a
            do while(parent*2 <= this%n)

                child = parent*2
                if (child + 1 <= this%n) then
                    if (x(child+1)%priority > x(child)%priority) then
                        child = child + 1
                    end if
                end if
                if (x(parent)%priority < x(child)%priority) then
                    x([child, parent]) = x([parent, child])
                    parent = child
                else
                    exit
                end if

            end do

        end associate

    end subroutine siftdown

    function pop(this) result(event)

        implicit none

        class(tevent_queue) :: this

        type(tevent) :: event

        if (this%n > 0) then
            event = this%buf(1)%event
            this%buf(1) = this%buf(this%n)
            this%n = this%n - 1
            if (this%n > 0) then
                call this%siftdown(1)
            end if
        else
            stop "tevent_queue%pop() - queue is empty"
        end if

    end function pop

    integer function len(this)

        implicit none

        class(tevent_queue) :: this

        len = this%n

    end function len

    logical function empty(this)

        implicit none

        class(tevent_queue) :: this

        empty = this%n < 1

    end function empty

    subroutine push(this, event, priority)

        implicit none

        class(tevent_queue) :: this

        type(tevent), intent(in) :: event
        real(wp), intent(in), optional :: priority

        type(tevent_node) :: node
        type(tevent_node), allocatable :: tmp(:)

        integer :: i

        node%event = event

        node%priority = -event%get_invoke_time() ! default priority
        if (present(priority)) node%priority = priority

        this%n = this%n + 1

        if (.not.allocated(this%buf)) allocate(this%buf(1))
        if (size(this%buf) < this%n) then
            allocate(tmp(2*size(this%buf)))
            tmp(1:this%n-1) = this%buf
            call move_alloc(tmp, this%buf)
        end if
        this%buf(this%n) = node
        i = this%n
        do
            i = i / 2
            if (i == 0) exit
            call this%siftdown(i)
        end do

    end subroutine push

end module event_queue