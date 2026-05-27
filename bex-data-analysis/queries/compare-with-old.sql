ATTACH DATABASE '/Users/nhejduk/Research-Local/blgt-parent/contracts-and-testing/bex/dbs/sqlite/teco2.sqlite3' AS old_db;

SELECT s1.configuration, s1.test_passed, s2.test_passed, s1.cmd_line_args, s2.cmd_line_args FROM snake as s1
JOIN old_db.snake as s2
ON s1.configuration=s2.configuration
AND s1.mutation_index=s2.mutation_index
AND s1.mutant_module=s2.mutant_module
AND s1.test_index=s2.test_index
AND s1.module_under_test=s2.module_under_test
AND s1.test_passed<>s2.test_passed