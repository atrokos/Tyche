module Money where
import Date (Date)
import Groups (Group)

data Transaction = Transaction {date::Date, from::Group, to::Group, money::Money}
data Money = Money {amount::Float, currency::String}

