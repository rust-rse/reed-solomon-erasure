# Reed-Solomon: Panic 消除与可恢复错误路径修复

日期: 2026-06-17

## 修复目标（对应 5 条高优先级 finding）

1. 确保 `Matrix` 关键构造与计算不再因非法维度直接 panic
2. `ReedSolomon::build_matrix` 与矩阵构建相关路径不再 panic
3. `ReedSolomon::get_data_decode_matrix` 及重建路径可恢复失败
4. GF(2^8) 与 GF(2^16) 除法/求逆在非法输入下可恢复返回
5. 通过文档把“panic 风险 → 错误码”链路固定化，便于后续治理与回归

## 改动清单（按优先级）

### P1-1 `src/matrix.rs`

- 将 `Matrix::new_with_data` 改为返回 `Result<Matrix<F>, matrix::Error>`。
  - 新增错误：`EmptyInput` / `InconsistentRowSizes`。
- 将 `Matrix::multiply` 改为返回 `Result<Matrix<F>, matrix::Error>`。
  - 新增错误：`IncompatibleDimensions`。
- 将 `Matrix::augment` 改为返回 `Result<Matrix<F>, matrix::Error>`。
  - 同上返回维度不一致。
- `Matrix::invert` 保持 `Result` 返回但改为返回 `Error::NonSquare` 而非 panic。
- `invert` 内部调用 `augment` 使用 `?`。
- 测试更新：将原本 `#[should_panic]` 的场景改为 `Err(...)` 断言。

### P1-2 `src/core/codec.rs`

- `ReedSolomon::build_matrix` 改为 `Result<Matrix<F>, Error>`。
- `build_matrix` 中对 `top.invert()` 与 `vandermonde.multiply()` 的失败统一映射为
  `Error::InvalidCustomMatrix`，去掉 panic。
- `build_cauchy_matrix` 对分母 `F::add(F::nth(r), F::nth(c)) == F::zero()` 做显式返回错误。
- `build_jerasure_like_matrix` 中以下关键路径加入零检查并返回 `Error::InvalidCustomMatrix`：
  - 对角线缩放分母为 0
  - 末行与下三角归一化分母为 0

### P1-3 `src/core/reconstruct.rs`

- `ReedSolomon::get_data_decode_matrix` 改为 `Result<Arc<Matrix<_>>, Error>`。
- 将 `sub_matrix.invert()` 的 `Err` 映射为 `Error::InvalidCustomMatrix`。
- 关键调用点改为 `?` 传播：
  - `reconstruct_internal`
  - `reconstruct_required_data_only`
- 增加 `valid_indices.len() != data_shard_count` 的防御检查，返回
  `Error::TooFewShardsPresent`。

### P1-4 `src/galois_8/mod.rs`

- `div(a, b)` 由 panic 改为零安全返回：`b == 0` 时返回 `0`。
- doc 注释同步更新说明行为。
- 测试由 `#[should_panic]` 改为返回值断言。

### P1-5 `src/galois_16.rs`

- `Element::polynom_div` 在 `rhs` 为 0 时返回 `(Element::zero(), self)`。
- `Element::inverse` 在零元输入时返回 `Element::zero()`。
- `inverse` 的 `gcd == 0` 分支返回 `Element::zero()`。
- 测试将 zero 除法 panic 用例改为 zero 结果断言。

## 行为差异说明

- 这次修复是“防 panic 化”而非“数学上扩展定义”：
  - 运行时不再因非法输入触发崩溃（panic），而是透传可恢复错误或定义行为。
  - 在 GF 运算层，非法除法/求逆返回 `0` 是兼容的降级策略，
    保证库面向 untrusted 输入不会直接终止进程。

## 建议下一步

1. 评估 `div(?,0) -> 0` 的行为是否满足业务语义：
   - 若希望保留严格数学语义，可增加 `try_div` API 作为显式失败通道。
2. 补齐文档化错误码映射表（`matrix::Error` -> `crate::Error`）并加入审计级回归测试。
3. 增加覆盖用例：
   - `reconstruct` 在 `data_decode_matrix` 不可逆时，返回 `InvalidCustomMatrix` 而非 panic。
   - 构造坏参数的 fuzz 场景（非法行长、非法维度、cauchy/jerasure 零分母）。

## 本轮“彻底修复”补充（已落盘代码同步）

- 已修复遗漏的签名收口问题：
  - `ReedSolomon::build_cauchy_matrix` 改为 `Result<Matrix<_>, Error>`。
  - `ReedSolomon::build_jerasure_like_matrix` 改为 `Result<Matrix<_>, Error>`。
  - `build_matrix_with_options` 中对应分支同步去 `Ok(...)` 包装，改为直接传播 `Err`。
- 风险处理收口：
  - 避免将会返回 `Err` 的路径保持 `Matrix` 返回值，消除了潜在编译与异常分支漏网点。
  - `ReedSolomon::build_matrix` 已持续返回 `Result`，并在 `with_options` 中保持 `?` 传播。

## 关键函数到修复动作（最终对照）

- `src/matrix.rs`
  - `new_with_data`：空输入/不一致行长改返回 `Err`。
  - `multiply`：维度不兼容返回 `Err`。
  - `augment`：行数不兼容返回 `Err`。
  - `invert`：非方阵返回 `Err(Error::NonSquare)`。
- `src/core/codec.rs`
  - `build_matrix`：`top.invert` 与 `multiply` 返回错误映射。
  - `build_cauchy_matrix`：分母为 0 时返回 `Err(Error::InvalidCustomMatrix)`。
  - `build_jerasure_like_matrix`：关键缩放分母为 0 时返回 `Err(Error::InvalidCustomMatrix)`。
  - `build_matrix_with_options`：所有分支统一为 `Result`。
- `src/core/reconstruct.rs`
  - `get_data_decode_matrix`：`Result` 化，错误可恢复上抛。
  - `reconstruct_internal` 与 `reconstruct_required_data_only`：添加 `?` 传播。
- `src/galois_16.rs`
  - `polynom_div`：右操作数为 0 返回 `(0, self)`。
  - `inverse`：零元与 `gcd == 0` 返回 `0`。
- `src/galois_8/mod.rs`
  - `div(a,b)`：`b == 0` 返回 `0`。

## 交付清单

- 变更文档：`docs/security-hardening-panic-elimination-2026-06-17.md`
- 变更文件：
  - `src/core/codec.rs`
  - `src/core/reconstruct.rs`
  - `src/galois_8/mod.rs`
  - `src/galois_8/tests.rs`
  - `src/galois_8/policy.rs`
  - `src/galois_16.rs`
  - `src/matrix.rs`
  - `src/tests/mod.rs`

## 回归验证计划清单（最小测试矩阵）

### 回归优先级与命令

- P0（上线前必跑）
  - `cargo test matrix -- --nocapture`
    - 覆盖：`matrix.rs` 全量构造/乘法/扩展/逆矩阵相关测试
    - 覆盖边界：`InconsistentRowSizes`、`IncompatibleDimensions`、`NonSquare`
  - `cargo test reconstruct -- --nocapture`
    - 覆盖：classic + leopard 路径、data-only/required-only、并行策略路径、reconstruct cache 边界
    - 回归目标：`get_data_decode_matrix` Result 化后在 `reconstruct` 主路径的传播行为
- P1（发布前建议跑）
  - `cargo test div_a_is_0 -- --nocapture`
    - 覆盖：`galois_8::tests::test_div_a_is_0`
  - `cargo test div_b_is_0 -- --nocapture`
    - 覆盖：`galois_8::tests::test_div_b_is_0`
    - 覆盖：`galois_16::tests::test_div_b_is_0`
    - 回归目标：非法除法输入不再 panic、返回降级值
- P2（回归增强）
  - `cargo test test_codec_options_accepts_cauchy_matrix_mode -- --nocapture`
  - `cargo test test_jerasure_like_matrix_mode_roundtrips_and_differs_from_vandermonde -- --nocapture`
  - `cargo test test_with_custom_matrix_rejects_too_few_rows -- --nocapture`
    - 重点确认 `InvalidCustomMatrix` 与 recover/构建路径一致

### 结果判定规则

- P0/P1 全通过：进入 P2
- P0/P1 任一失败：冻结并将失败测试名、环境（`rustc/cargo --version`、运行命令、目标 triple）写入回归附录
- 与本次修复直接相关失败应优先修复：
  - `get_data_decode_matrix` 未解包导致 panic/类型错误
  - `build_cauchy_matrix`/`build_jerasure_like_matrix` 未返回 `Err` 分支（或签名不一致）
  - `GF` 除法/求逆在零输入下仍触发 panic

### 一页式验收清单（可直接贴 PR 描述）

- [ ] matrix 类用例全部通过
- [ ] reconstruct（含 required/data-only/parity）关键路径通过
- [ ] galois_8 零输入除法行为通过
- [ ] galois_16 零输入除法/零求逆行为通过
- [ ] 已记录并提交变更：
  - [ ] `src/matrix.rs`
  - [ ] `src/core/codec.rs`
  - [ ] `src/core/reconstruct.rs`
  - [ ] `src/galois_8/mod.rs`
  - [ ] `src/galois_8/policy.rs`
  - [ ] `src/galois_16.rs`
  - [ ] `src/tests/mod.rs`

## CI 建议命令清单（可直接落流水线）

### P0（必跑）
1. `cargo fmt --all -- --check`
2. `cargo check --all-targets`
3. `cargo test matrix -- --nocapture`
4. `cargo test reconstruct -- --nocapture`
5. `cargo test div_a_is_0 -- --nocapture`
6. `cargo test div_b_is_0 -- --nocapture`

### P1（建议跑，回归闭环）
1. `cargo test matrix --all-features -- --nocapture`
2. `cargo test reconstruct --all-features -- --nocapture`
3. `cargo test test_codec_options_accepts_cauchy_matrix_mode test_codec_options_accepts_jerasure_like_matrix_mode test_with_custom_matrix_rejects_too_few_rows -- --nocapture`

### P2（按机器配置补跑）
1. `RSE_BACKEND_OVERRIDE=scalar cargo test reconstruct -- --nocapture`
2. `RSE_BACKEND_OVERRIDE=rust-gfni-avx2 cargo test reconstruct -- --nocapture`
3. `RSE_BACKEND_OVERRIDE=rust-avx2 cargo test reconstruct -- --nocapture`

- 已落盘流水线：`.github/workflows/security-hardening-regression.yml`

- 与现有 `ci.yml` 解耦（仅聚焦安全回归），便于在 PR 中按需启用。

- 工作流触发方式：
  - PR 事件默认执行 P0 + P1 + P2。
  - 手动触发支持 `run_level`（`P0|P1|P2`）
    - 选 `P0`：仅跑基础层
    - 选 `P1`：跑 P0 + P1
    - 选 `P2`：跑 P0 + P1 + P2（默认）

- 失败即停建议
  1. 若某命令失败，先只补跑失败命令并附带 `-Z timings` 等诊断上下文。
  2. P0 失败必须修复后再继续，P1/P2 可作为可选门控。
  3. 回写结果到回归附录：命令、失败测试名、`rustc`、`cargo` 版本与机器架构。
