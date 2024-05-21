# Tyche
A simple ledger in Haskell.

## Sessions
Session is a single CSV file where all data are stored.
To create initialize the program and create a new session, type:\
```powershell
init <session name>
```

To create another session and switch to it, or switch to an existing one, type:\
```powershell
switch <session name>
```
To see current session, type
```powershell
session
```

## Transactions
### Add transaction
To add a new transaction, type:
```powershell
add <date> <name> <from> <to> <amount> <currency>
```

Where:
- The date is in the following format: `YYYY-(M)M-(D)D`
- `from` and `to` are Groups (see below)
- `amount` is the monetary amount, optionally with a dot separator
- `currency` is the three-letter ISO code of the currency

Example:
```powershell
add 2024-05-07 "An example" main::incomes expenses::examples -256 CZK
```


### Remove transaction
To remove a transaction, type:
```powershell
remove [filters]
```

Where:
- `[filters]` is a list of filters (see below)

Example:
```powershell
remove --title eq "An example" --from eq main::incomes
```

If some filters would remove all of your transaction, the program will ask you if you
want to proceed.


## Groups
Groups make it possible to "tag" or denote your transactions. A group can have infinitely many
subgroups, which are denoted by `::`. Examples:
- `expenses::food`
- `incomes::work::some_job`

Two groups that share at least the first group name are considered to be in the same group.
This is useful when using filters:

For example, say you have 3 groups: `expenses::food`, `expenses::drinks` and `incomes`. Calling
```powershell
filter --to eq "expenses"
```
will show you all transactions that have their `to` group set to either `expenses::food` or `expenses::drinks`.

## Filters
Filters are a powerful feature that allows you to narrow down transactions based on specific criteria. They are easy to use and follow a simple syntax.

### Syntax and Usage
To apply a filter, use the following syntax pattern:
```powershell
filter –-<property> {gt, ge, lt, le, eq} <value>
```
Where:
- `--<property>` is the transaction property you want to filter by (e.g., `--amount`, `--date`, `--title`).
- `{gt, ge, lt, le, eq}` are the operators representing greater than (`gt`), greater than or equal to (`ge`), less than (`lt`), less than or equal to (`le`), and equal to (`eq`).
- `<value>` is the value you want to compare the property against.

### Examples
#### Filtering by Amount
To display transactions where the amount is greater than 50:

```powershell
filter --amount gt 50
```

#### Filtering by From
To display transactions where the from group is starting with "incomes" and the value is greater than 2000:

```powershell
filter --amount gt 2000 --from eq "incomes"
```

## Statistics
Getting statistics from a CSV file is exactly the same as with filtering.
The only difference is that the program returns all total incomes (values > 0), expenses
(values < 0) and their difference.

### Example
#### Filtering by Amount
This will compute statistics on all transactions that have their amount less than -50:

```powershell
stats --amount lt 50
```