program main

    use simulation
    use simulation_record

    implicit none

    integer, parameter :: dp = kind(1.0d0)

    type(tsimulation) :: simulation
    type(tsimulation_record) :: simulation_record

    call simulation%init(5, 8000, 1._dp, 1, 1._dp, 1, 1._dp, 1, 5)
    call simulation%run()

    simulation_record = simulation%get_simulation_record()
    call simulation_record%log_event_records("event_records.csv")
    call simulation_record%log_customer_records("customer_records.csv")
    call simulation_record%show_stats()
    print '()'
    call simulation%show_theoretical_stats()
end program