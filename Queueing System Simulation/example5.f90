! Задача.
! ------
! Одноканальная СМО - ЭВМ, на которую поступают заявки (требования на расчеты). Поток заявок - простейший со
! средним интервалом между заявками 10 мин. Время обслуживания распределено по закону Эрланга 3-го порядка с
! математическим ожиданием 8 мин.
! ------
! Определить среднее число заявок в СМО и среднее число заявок в очереди, а также средние времена пребывания
! заявки в системе и в очереди.

program main

    use simulation
    use simulation_record

    implicit none

    integer, parameter :: dp = kind(1.0d0)

    type(tsimulation) :: simulation
    type(tsimulation_record) :: simulation_record

    call simulation%init(1, 14000, 10._dp, 1, 8._dp, 3)
    call simulation%run()

    simulation_record = simulation%get_simulation_record()
    call simulation_record%log_event_records("event_records.csv")
    call simulation_record%log_customer_records("customer_records.csv")
    call simulation_record%show_stats()
    print '()'
    call simulation%show_theoretical_stats()

end program