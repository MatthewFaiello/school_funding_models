/*
Purpose:
    Create one school-level row with Enrollment, LI, MLL, K-8 enrollment,
    and Grade 10 enrollment counts aligned to the unit-count reporting
    structure.

Definitions:
    - Enrollment: distinct students assigned to the unit-count school.
    - LI: students with LowIncome = 'LOWINC'.
    - MLL: students with active ELL status, ELL = 'ELL'.
      ELM, ELX, and N_ELL are excluded.
    - K8Enrollment: students in Kindergarten through Grade 8.
      Pre-K students are excluded.
    - Grade10Enrollment: students in Grade 10.

Important:
    UnitCount_District and UnitCount_School are used instead of the
    ordinary DistrictCode and SchoolCode fields so the output aligns
    with the Needs-Based Unit Enrollment Summary.
*/

DECLARE @SchoolYear int = 2026;

-- Informational reporting date only. The unit-count table already represents
-- the official September 30 snapshot for the selected school year. This
-- variable labels the export and does not filter the source rows.
DECLARE @ReportingCountDate date = '2025-09-30';


WITH school_counts AS (
    SELECT
        u.SchoolYear,

        DistrictCode =
            u.UnitCount_District,

        SchoolCode =
            u.UnitCount_School,

        Enrollment =
            COUNT(
                DISTINCT u.StudentID
            ),

        LI =
            COUNT(
                DISTINCT CASE
                    WHEN u.LowIncome = 'LOWINC'
                        THEN u.StudentID
                END
            ),

        MLL =
            COUNT(
                DISTINCT CASE
                    WHEN u.ELL = 'ELL'
                        THEN u.StudentID
                END
            ),

        K8Enrollment =
            COUNT(
                DISTINCT CASE
                    WHEN u.Grade IN (
                        'KN',
                        '01',
                        '02',
                        '03',
                        '04',
                        '05',
                        '06',
                        '07',
                        '08'
                    )
                        THEN u.StudentID
                END
            ),

        Grade10Enrollment =
            COUNT(
                DISTINCT CASE
                    WHEN u.Grade = '10'
                        THEN u.StudentID
                END
            )

    FROM PUBLICREPORTMART.details.P20_STUDENT_ENROLLMENT_UNITCOUNT AS u

    WHERE
        u.SchoolYear = @SchoolYear
        AND u.UnitCount_District IS NOT NULL
        AND u.UnitCount_School IS NOT NULL

    GROUP BY
        u.SchoolYear,
        u.UnitCount_District,
        u.UnitCount_School
),

district_names AS (
    SELECT
        d.SchoolYear,
        d.DistrictCode,
        DistrictName =
            MAX(
                d.DistrictName
            )

    FROM CodeLibrary.dbo.District AS d

    WHERE
        d.SchoolYear = @SchoolYear

    GROUP BY
        d.SchoolYear,
        d.DistrictCode
),

school_names AS (
    SELECT
        s.SchoolYear,
        s.DistrictCode,
        s.SchoolCode,
        SchoolName =
            MAX(
                s.SchoolName
            )

    FROM CodeLibrary.dbo.School AS s

    WHERE
        s.SchoolYear = @SchoolYear

    GROUP BY
        s.SchoolYear,
        s.DistrictCode,
        s.SchoolCode
)


SELECT
    AggregationLevel =
        'School',

    sc.SchoolYear,

    CountDate =
        @ReportingCountDate,

    sc.DistrictCode,
    dn.DistrictName,

    sc.SchoolCode,
    sn.SchoolName,

    sc.Enrollment,
    sc.LI,
    sc.MLL,
    sc.K8Enrollment,
    sc.Grade10Enrollment

FROM school_counts AS sc

LEFT JOIN district_names AS dn
    ON  sc.SchoolYear = dn.SchoolYear
    AND sc.DistrictCode = dn.DistrictCode

LEFT JOIN school_names AS sn
    ON  sc.SchoolYear = sn.SchoolYear
    AND sc.DistrictCode = sn.DistrictCode
    AND sc.SchoolCode = sn.SchoolCode

ORDER BY
    dn.DistrictName,
    sn.SchoolName,
    sc.DistrictCode,
    sc.SchoolCode;