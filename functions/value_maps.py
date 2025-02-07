sex: dict[int | None, str | None] = {
    None: None,
    0: "female",
    1: "male",
}

ethnic_background: dict[int | None, str | None] = {
    None: None,
    -3: None,  # Prefer not to answer
    -1: None,  # Do not know
    1: "white",  # White
    2: "mixed",  # Mixed
    3: "asian",  # Asian or Asian British
    4: "black",  # Black or Black British
    5: "asian",  # Chinese
    6: "other",  # Other ethnic group
    1001: "white",  # British
    1002: "white",  # Irish
    1003: "white",  # Any other white background
    2001: "mixed",  # White and Black Caribbean
    2002: "mixed",  # White and Black African
    2003: "mixed",  # White and Asian
    2004: "mixed",  # Any other mixed background
    3001: "asian",  # Indian
    3002: "asian",  # Pakistani
    3003: "asian",  # Bangladeshi
    3004: "asian",  # Any other Asian background
    4001: "black",  # Caribbean
    4002: "black",  # African
    4003: "black",  # Any other Black background
}

smoking_status: dict[int | None, str | None] = {
    None: None,
    -3: None,  # Prefer not to answer
    0: "never",  # Never
    1: "ever",  # Previous
    2: "ever",  # Current
}

alcohol_drinker_status: dict[int | None, str | None] = {
    None: None,
    -3: None,  # Prefer not to answer
    0: "never",  # Never
    1: "ever",  # Previous
    2: "ever",  # Current
}

value_maps: dict[str, dict[int | None, str | None]] = {
    "sex": sex,
    "ethnic_background": ethnic_background,
    "smoking_status": smoking_status,
    "alcohol_drinker_status": alcohol_drinker_status,
}
