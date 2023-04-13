#!/usr/bin/env python3
"""
Calculate proceeds per mining share

Take csv output from mining activity and calculate cost basis for shares sold.

https://www.fairspot.host/hnt-export-mining-tax

"type,buyAmount,buyCurrency,sellAmount,sellCurrency,feeAmount,feeCurrency,exchange,tradeGroup,comment,date,txId,buyValueUSD,sellValueUSD,Date Sold,Proceeds"
"""

import sys
import csv
import argparse
from datetime import date

def calculate_proceeds(csv_file_path, proceeds_received=0, shares_sold=0, date_sold="1/1/1", ttax=False, filter=False):
    processed_data = []

    total_records = 0
    total_shares = 0
    total_proceeds = 0
    total_cost_basis = 0

    with open(csv_file_path, "r") as csv_file:
        csv_reader = csv.reader(csv_file)
        header = next(csv_reader)

        for row in csv_reader:
            if not row:
                break

            if row[0] != "Mining" or total_shares >= shares_sold or row[-1]:
                if not filter:
                        processed_data.append(row)
            else:
                buy_amount = float(row[1])
                percent_of_total_shares = (shares_sold - (shares_sold - buy_amount))/shares_sold
                proceeds_per_share = round(percent_of_total_shares * proceeds_received,2)
                processed_data.append(row[:-2] + [date_sold, proceeds_per_share])
                total_shares += buy_amount
                total_proceeds += proceeds_per_share
                total_cost_basis += float(row[12])
                total_records += 1

    ttax_headers = [('Asset Name', 'buyCurrency'),
                    ('Received Date', 'date'),
                    ('Cost Basis (USD)', 'buyValueUSD'),
                    ('Date Sold', 'Date Sold'),
                    ('Proceeds', 'Proceeds'),
                    ('Amount Sold', 'buyAmount'),
                    ]

    if ttax:
        print(",".join([h[0] for h in ttax_headers]))
        for row in processed_data:
            print(",".join([str(row[header.index(h[1])]) for h in ttax_headers]))
    else:
        print(",".join(header))
        for row in processed_data:
            print(",".join([str(x) for x in row]))

    print(f"\nTotal Shares: {total_shares}\nTotal Proceeds: {total_proceeds}\nTotal Cost Basis: {total_cost_basis}\nTotal Records: {total_records}", file=sys.stderr)

    if total_shares < shares_sold:
        print("Error: Not enough HNT transactions to satisfy cost basis", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("csv_file_path", default="file.csv", help="File path of the CSV file containing the mining share data")
    parser.add_argument("-p", "--proceeds_received", type=float, default=100, help="Amount of proceeds received for the shares sold")
    parser.add_argument("-s", "--shares_sold", type=int, default=100, help="Number of shares sold")
    parser.add_argument("-d", "--date_sold", type=str, default=date.today().strftime("%m/%d/%Y"), help="Date of shares sold (MM/DD/YYYY)")
    parser.add_argument("-t", "--ttax", action='store_true', help="Generate ttax import output")
    parser.add_argument("-f", "--filter", action='store_true', help="Filter output to processed data")

    args = parser.parse_args()

    calculate_proceeds(args.csv_file_path, args.proceeds_received, args.shares_sold, args.date_sold, args.ttax, args.filter)
