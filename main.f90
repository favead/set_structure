program test_set_sum
    use type_set
    use type_point
    implicit none
    real :: x1 = 0.2, y1 = 0.4, x2 = 0.3, y2 = 0.5
    type(Point) :: p1,p2,p3,p4
    type(Point) :: points_1(3),points_2(2)
    type(Set) :: s3,s1,s2

    p1%x = x1
    p1%y = y1

    p2%x = x1
    p2%y = x2

    p3%x = x2
    p3%y = y1

    p4%x = x1
    p4%y = y2


    points_1 = [p1,p2,p4]
    points_2 = [p1,p2,p3]

    s1 = Set(points_1)
    s2 = Set(points_2)

    s3 = s1 + s2

    print*, s3%points !0.2,0.4; 0.3,0.5; 0.3,0.4; 0.2,0.5
end program
