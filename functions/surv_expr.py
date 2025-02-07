import polars as pl
from polars import col, lit, when


def surv_expr(surv: str, yr: int | float) -> pl.Expr:
    """
    Creates a polars expression to determine whether an individual is an event
    or censored within a specified number of years for a given survival outcome.

    Parameters
    ----------
    surv
        Column name representing the survival indicator (e.g., "os" or "css").
    yr
        The time horizon in years within which to determine the event status.

    Returns
    -------
    polars.Expr
        A polars expression that assigns 1 if the individual experienced the
        event within the specified timeframe, 0 if the individual was censored,
        and NULL otherwise.
    """
    expr: pl.Expr = (
        # Patients who are dead before the given time are considered as events
        when((col(surv) == 1) & (col(f"{surv}_time") <= (yr * 365.25)))
        .then(lit(1))
        # Patients who are dead after the given time are considered as censored
        .when((col(surv) == 1) & (col(f"{surv}_time") > (yr * 365.25)))
        .then(lit(0))
        # Patients who are alive after the given time are considered as censored
        .when((col(surv) == 0) & (col(f"{surv}_time") > (yr * 365.25)))
        .then(lit(0))
        # Other situations are considered as missing
        .otherwise(lit(None))
        .alias(f"{surv}_{yr}yr")
    )
    return expr
