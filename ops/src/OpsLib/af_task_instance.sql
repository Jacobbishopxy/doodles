CREATE TABLE public.task_instance (
	task_id varchar(250) NOT NULL,
	dag_id varchar(250) NOT NULL,
	run_id varchar(250) NOT NULL,
	map_index int4 DEFAULT '-1'::integer NOT NULL,
	start_date timestamptz NULL,
	end_date timestamptz NULL,
	duration float8 NULL,
	state varchar(20) NULL,
	try_number int4 NULL,
	max_tries int4 DEFAULT '-1'::integer NULL,
	hostname varchar(1000) NULL,
	unixname varchar(1000) NULL,
	job_id int4 NULL,
	pool varchar(256) NOT NULL,
	pool_slots int4 NOT NULL,
	queue varchar(256) NULL,
	priority_weight int4 NULL,
	"operator" varchar(1000) NULL,
	queued_dttm timestamptz NULL,
	queued_by_job_id int4 NULL,
	pid int4 NULL,
	executor_config bytea NULL,
	updated_at timestamptz NULL,
	external_executor_id varchar(250) NULL,
	trigger_id int4 NULL,
	trigger_timeout timestamp NULL,
	next_method varchar(1000) NULL,
	next_kwargs json NULL,
	CONSTRAINT task_instance_pkey PRIMARY KEY (dag_id, task_id, run_id, map_index)
);
CREATE INDEX ti_dag_run ON task_instance USING btree (dag_id, run_id);
CREATE INDEX ti_dag_state ON task_instance USING btree (dag_id, state);
CREATE INDEX ti_job_id ON task_instance USING btree (job_id);
CREATE INDEX ti_pool ON task_instance USING btree (pool, state, priority_weight);
CREATE INDEX ti_state ON task_instance USING btree (state);
CREATE INDEX ti_state_lkp ON task_instance USING btree (dag_id, task_id, run_id, state);
CREATE INDEX ti_trigger_id ON task_instance USING btree (trigger_id);

----------------------------------------------------------------------------------------------------

CREATE TABLE public.xcom (
	dag_run_id int4 NOT NULL,
	task_id varchar(250) NOT NULL,
	map_index int4 DEFAULT '-1'::integer NOT NULL,
	"key" varchar(512) NOT NULL,
	dag_id varchar(250) NOT NULL,
	run_id varchar(250) NOT NULL,
	value bytea NULL,
	"timestamp" timestamptz NOT NULL,
	CONSTRAINT xcom_pkey PRIMARY KEY (dag_run_id, task_id, map_index, key)
);
CREATE INDEX idx_xcom_key ON xcom USING btree (key);
CREATE INDEX idx_xcom_task_instance ON xcom USING btree (dag_id, task_id, run_id, map_index);


-- public.xcom foreign keys

ALTER TABLE public.xcom ADD CONSTRAINT xcom_task_instance_fkey FOREIGN KEY (dag_id,task_id,run_id,map_index) REFERENCES public.task_instance(dag_id,task_id,run_id,map_index) ON DELETE CASCADE;

