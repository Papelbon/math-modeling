program main

    use ogpf
    use random
    use cdf_chisq_mod

    implicit none

    type(gpf) :: gp
    type(trand) :: rand

    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: m = 9, n = 12, amount = 22822

    integer :: i, j, status
    integer :: counts(1:m,1:n), counts_1st(1:m), counts_2nd(1:n)

    real(dp) :: prob(1:m,1:n), empirical(1:m,1:n)
    real(dp) :: vals_1st(1:m), vals_2nd(1:n)
    real(dp) :: samples(1:2, 1:amount)
    real(dp) :: exp_1st, exp_2nd, var_1st, var_2nd
    real(dp) :: conf_int_exp_1st(2), conf_int_exp_2nd(2), conf_int_var_1st(2), conf_int_var_2nd(2)
    real(dp) :: conf_level, sign_level, chisq_stats, p_value

    ! For plotting
    real(dp) :: x(100), y(100)
    real(dp), allocatable:: xgrid(:,:), ygrid(:,:)

    ! Initialization --------------------------------------------------------------------------------------------------

    call random_number(prob)
    prob = prob/sum(prob)
    call random_number(vals_1st)
    call random_number(vals_2nd)

    ! Output theoretical probability matrix, 1st RV vals and 2nd RV vals ----------------------------------------------

    print "(/, a31)", "Theoretical probability matrix:"
    do i = 1, m
        print '(20f10.5)', (prob(i,j), j = 1,n)
    end do

    print "(/, a27)", "1st random variable values:"
    print '(20f10.5)', (vals_1st(i), i = 1,m)

    print "(/, a27)", "2nd random variable values:"
    print '(20f10.5)', (vals_2nd(j), j = 1,n)

    ! Compute empirical probability matrix and random variables values stats ------------------------------------------

    call rand%d2Drv_matrix(prob, vals_1st, vals_2nd, amount, empirical, samples, counts)

    counts_1st = sum(counts, dim=2)
    counts_2nd = sum(counts, dim=1)

    ! sum(counts)        equ amount
    ! sum(counts_1st)    equ amount
    ! sum(counts_2nd)    equ amount
    ! size(samples(1,:)) equ amount
    ! size(samples(2,:)) equ amount

    ! Output random variables values stats and empirical probability matrix -------------------------------------------

    print "(/, a29)", "Empirical probability matrix:"
    do i = 1, m
        print '(20f10.5)', (empirical(i,j), j = 1,n)
    end do

    print "(/, a30)", "Random variables values stats:"
    print "(a10)", "- general:"
    do i = 1, m
        print '(20i10)', (counts(i,j), j = 1,n)
    end do
    print "(/, a9)", "- 1st RV:"
    print '(20i10)', counts_1st
    print '(a2)', "--"
    print '(20f10.5)', sum(empirical,dim=2)
    print "(/, a9)", "- 2nd RV:"
    print '(20i10)', counts_2nd
    print '(a2)', "--"
    print '(20f10.5)', sum(empirical,dim=1)

    ! Plot theoretical and empirical probability matrices -------------------------------------------------------------

    call meshgrid(xgrid, ygrid, vals_1st, vals_2nd)

    call gp%title('Theoretical probability matrix')
    call gp%surf(xgrid, ygrid, prob, 'with impulses lw 7 lc "blue"')
    call gp%surf(xgrid, ygrid, prob, palette='jet')

    call gp%title('Empirical probability matrix')
    call gp%surf(xgrid, ygrid, empirical, 'with impulses lw 7 lc "blue"')
    call gp%surf(xgrid, ygrid, empirical, palette='jet')

    ! Compute and output point estimations ----------------------------------------------------------------------------

    print "(/, a23)", "Population expectation:"
    print '(20f10.5)', rand%expectation(sum(prob, dim=2), vals_1st), rand%expectation(sum(prob, dim=1), vals_2nd)

    print "(/, a19)", "Sample expectation:"
    exp_1st = rand%expectation(samples(1,:))
    exp_2nd = rand%expectation(samples(2,:))
    print '(20f10.5)', exp_1st, exp_2nd

    print "(/, a20)", "Population variance:"
    print '(20f10.5)', rand%variance(sum(prob, dim=2), vals_1st), rand%variance(sum(prob, dim=1), vals_2nd)

    print "(/, a16)", "Sample variance:"
    print '(20f10.5)', rand%variance(samples(1,:)), rand%variance(samples(2,:))

    ! Compute and output interval estimations -------------------------------------------------------------------------

    conf_level = 0.95_dp
    print "(/, a17)", "Confidence level:"
    print '(20f10.5)', conf_level

    print "(/, a43)", "Confidence interval for sample expectation:"
    print "(a9)", "- 1st RV:"
    conf_int_exp_1st = rand%conf_int_exp(samples(1,:), conf_level)
    print '(20f10.5)', conf_int_exp_1st
    print "(/, a9)", "- 2nd RV:"
    conf_int_exp_2nd = rand%conf_int_exp(samples(2,:), conf_level)
    print '(20f10.5)', conf_int_exp_2nd

    print "(/, a40)", "Confidence interval for sample variance:"
    print "(a9)", "- 1st RV:"
    print '(20f10.5)', rand%conf_int_var(samples(1,:), conf_level)
    print "(/, a9)", "- 2nd RV:"
    print '(20f10.5)', rand%conf_int_var(samples(2,:), conf_level)

    ! Plot interval estimations ---------------------------------------------------------------------------------------

    x(1:1) = exp_1st
    y(1:2) = maxval(sum(empirical,dim=2))
    call gp%title('Confidence interval for expectation (1st RV)')
    call gp%plot(x1=vals_1st, y1=sum(empirical,dim=2), ls1='t "Probability" with impulses lw 3 lc rgb "#4285f4"', &
            x2=conf_int_exp_1st, y2=y(1:2), ls2='t "CI bounds" with impulses lc rgb "#39b979"', &
            x3=x(1:1), y3=y(1:1), ls3='t "Expectation" with impulses lc rgb "#db4437"')

    x(1:1) = exp_2nd
    y(1:2) = maxval(sum(empirical,dim=1))
    call gp%title('Confidence interval for expectation (2nd RV)')
    call gp%plot(x1=vals_2nd, y1=sum(empirical,dim=1), ls1='t "Probability" with impulses lw 3 lc rgb "#4285f4"', &
            x2=conf_int_exp_2nd, y2=y(1:2), ls2='t "CI bounds" with impulses lc rgb "#39b979"', &
            x3=x(1:1), y3=y(1:1), ls3='t "Expectation" with impulses lc rgb "#db4437"')

    ! Compute and output correlation coefficient ----------------------------------------------------------------------

    print "(/, a35)", "Population correlation coefficient:"
    print '(20f10.5)', rand%correlation(prob, vals_1st, vals_2nd)

    print "(/, a31)", "Sample correlation coefficient:"
    print '(20f10.5)', rand%correlation(samples)

    ! Compute and output chi-square statistics -----------------------------------------------------------------------

    sign_level = 0.05_dp
    print "(/, a19)", "Significance level:"
    print '(f10.5)', sign_level

    print "(/, a16)", "Chi-square test:"
    print "(a12)", "- statistic:"
    ! Chi-square distribution with (m−1)(n-1) degrees of freedom
    chisq_stats = sum(((empirical-prob)**2)/prob)*amount
    print '(f10.5)', chisq_stats
    print "(a10)", "- p-value:"
    p_value = ccum_chisq(x=chisq_stats, df=real((m-1)*(n-1),dp))
    print '(f10.5)', ccum_chisq(x=chisq_stats, df=real((m-1)*(n-1),dp))
    if (p_value > sign_level) then
        print '(a44)', "The result is statistically non-significant."
    else
        print '(a40)', "The result is statistically significant."
    end if

end program