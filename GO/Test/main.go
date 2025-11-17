package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type Table struct {
	name   string
	values []map[string]string
}

func readCSVFile(filename string) (Table, error) {
	f, err := os.Open(filename)
	if err != nil {
		return Table{}, err
	}
	defer f.Close()

	reader := csv.NewReader(f)
	records, err := reader.ReadAll()
	if err != nil {
		return Table{}, err
	}

	if len(records) < 1 {
		return Table{}, fmt.Errorf("empty CSV file: %s", filename)
	}

	headers := records[0]
	rows := []map[string]string{}

	for _, record := range records[1:] {
		row := map[string]string{}
		for i, val := range record {
			if i < len(headers) {
				row[headers[i]] = val
			}
		}
		rows = append(rows, row)
	}

	tableName := filepath.Base(filename)
	tableName = tableName[:len(tableName)-len(filepath.Ext(tableName))]

	return Table{name: tableName, values: rows}, nil
}

type CSVReader struct {
	tables map[string]Table
}

func NewCSVReader() *CSVReader {
	return &CSVReader{tables: map[string]Table{}}
}

func (r *CSVReader) AddTable(filename string) bool {
	table, err := readCSVFile(filename)
	if err != nil {
		fmt.Println("Error:", err)
		return false
	}
	r.tables[table.name] = table
	return true
}

func (r *CSVReader) ExecuteQuery(query string) {
	query = strings.TrimSpace(query)
	query = strings.ReplaceAll(query, "\n", " ")

	selectIdx := strings.Index(strings.ToUpper(query), "SELECT ")
	fromIdx := strings.Index(strings.ToUpper(query), " FROM ")
	if selectIdx == -1 || fromIdx == -1 {
		fmt.Println("Invalid query")
		return
	}
	selectPart := strings.TrimSpace(query[selectIdx+7 : fromIdx])
	columns := strings.Split(selectPart, ",")
	for i := range columns {
		columns[i] = strings.TrimSpace(columns[i])
	}

	rest := query[fromIdx+6:]
	parts := strings.Fields(rest)
	if len(parts) < 1 {
		fmt.Println("Missing FROM table")
		return
	}
	table1Name := parts[0]
	table1, ok := r.tables[table1Name]
	if !ok {
		fmt.Printf("Table '%s' not found\n", table1Name)
		return
	}

	joinIdx := strings.Index(strings.ToUpper(rest), "JOIN ")
	if joinIdx == -1 {
		for _, row := range table1.values {
			for _, col := range columns {
				tParts := strings.Split(col, ".")
				var val string
				if len(tParts) == 2 {
					val = row[tParts[1]]
				} else {
					val = row[col]
				}
				fmt.Printf("%s\t", val)
			}
			fmt.Println()
		}
		return
	}

	joinPart := rest[joinIdx+5:]
	joinParts := strings.SplitN(joinPart, " ON ", 2)
	if len(joinParts) < 2 {
		fmt.Println("Invalid JOIN syntax")
		return
	}
	table2Name := strings.Fields(joinParts[0])[0]
	table2, ok := r.tables[table2Name]
	if !ok {
		fmt.Printf("Table '%s' not found\n", table2Name)
		return
	}

	onClause := strings.TrimSpace(joinParts[1])
	onParts := strings.Split(onClause, "=")
	if len(onParts) != 2 {
		fmt.Println("Invalid ON clause")
		return
	}
	left := strings.TrimSpace(onParts[0])
	right := strings.TrimSpace(onParts[1])
	leftParts := strings.Split(left, ".")
	rightParts := strings.Split(right, ".")

	leftCol := leftParts[1]
	rightCol := rightParts[1]

	for _, row1 := range table1.values {
		for _, row2 := range table2.values {
			if row1[leftCol] == row2[rightCol] {
				for _, col := range columns {
					tParts := strings.Split(col, ".")
					if len(tParts) != 2 {
						fmt.Printf("NULL\t")
						continue
					}
					tbl, colName := tParts[0], tParts[1]
					var val string
					if tbl == table1.name {
						val = row1[colName]
					} else if tbl == table2.name {
						val = row2[colName]
					}
					fmt.Printf("%s\t", val)
				}
				fmt.Println()
			}
		}
	}
}

func main() {
	r := NewCSVReader()
	r.AddTable("A.csv")
	r.AddTable("B.csv")

	query := `SELECT A.name, A.surname, B.age FROM A JOIN B ON A.surname = B.surname`
	r.ExecuteQuery(query)
}
