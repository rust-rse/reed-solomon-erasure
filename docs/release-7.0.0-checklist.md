# reed-solomon-erasure 7.0.0 发布清单（当前工作区）

本清单用于 7.0.0 的发布收口，不包含额外行为验证，只确保可复现的发布闸门可执行且可复查。

## 1. 版本冻结

- `CHANGELOG.md` 的 `7.0.0` 条目从 `Unreleased` 固化为正式日期。
- `Cargo.toml` 与文档中的版本号与发布说明保持一致。
- release 之前确认工作树无未提交变更（`git status --short` 为空）。

## 2. 发布模式定义

- `VALIDATION_PROFILE=fast`
  - 仅执行基本检查：`cargo check`、`selftest`、基础 smoke、no-default / std 两个 feature 检查。
- `VALIDATION_PROFILE=extended`
  - 在 fast 的基础上额外执行：smoke 回归门禁、backend 一致性（可选）、SIMD/override（可选）、小文件与重建热点门禁（可选）。
- `VALIDATION_PROFILE=release`
  - 开启 extended。
- 在 `release` 模式下，脚本会默认打开：
  - `RUN_BACKEND_CONSISTENCY=1`
  - `RUN_SMALL_FILE_GATE=1`
  - `RUN_RECONSTRUCTION_HOTSPOT_GATE=1`
  - `RUN_SIMD_ACCEL_TESTS=1`
- 在 `release` 模式下，相关基线文件缺失会直接失败（避免“只跑到结果但不比对”）。
- CI 中的 `release-preflight` 在标签推送时会先读取仓库变量：
  - `RSE_SMOKE_BASELINE`
  - `RSE_SMALL_FILE_BASELINE`
  - `RSE_RECONSTRUCTION_HOTSPOT_BASELINE`

  如果三项变量都已配置则以 `release` 模式运行；若任一缺失则自动降级为 `extended`。

### 2.1 GitHub 仓库变量说明

- 在仓库 Settings → Secrets and variables → Actions → Variables 中新增以下变量（推荐使用版本化对象存储路径）：
  - `RSE_SMOKE_BASELINE`
  - `RSE_SMALL_FILE_BASELINE`
  - `RSE_RECONSTRUCTION_HOTSPOT_BASELINE`

示例值：
- `RSE_SMOKE_BASELINE=artifacts/benchmarks/7.0.0/smoke-results.json`
- `RSE_SMALL_FILE_BASELINE=artifacts/benchmarks/7.0.0/small-file-results.json`
- `RSE_RECONSTRUCTION_HOTSPOT_BASELINE=artifacts/benchmarks/7.0.0/reconstruction-hotspot-results.json`

### 2.2 变量初始化（GitHub CLI）

在发布前可直接执行：

```bash
OWNER=houseme
REPO=reed-solomon-erasure
VER=7.0.0

gh variable set RSE_SMOKE_BASELINE "artifacts/benchmarks/${VER}/smoke-results.json" --repo ${OWNER}/${REPO}
gh variable set RSE_SMALL_FILE_BASELINE "artifacts/benchmarks/${VER}/small-file-results.json" --repo ${OWNER}/${REPO}
gh variable set RSE_RECONSTRUCTION_HOTSPOT_BASELINE "artifacts/benchmarks/${VER}/reconstruction-hotspot-results.json" --repo ${OWNER}/${REPO}
```

如需更新现有变量直接复用同一命令（同名变量会被覆盖）。若环境未登录 gh，可改为直接在仓库 Settings 页面手工维护变量。

## 3. 一页式 Tag 到发布（可直接贴 Release Note）

### 3.1 现场命令

```bash
cd /Users/zhi/Documents/code/rust/rustfs/reed-solomon-erasure

# 前置：仓库变量已按 2.1 配置
git tag -a v7.0.0 -m "release: v7.0.0"
git push origin v7.0.0
```

### 3.2 发布预检

```bash
cd /Users/zhi/Documents/code/rust/rustfs/reed-solomon-erasure
export VALIDATION_PROFILE=release
export RSE_SMOKE_BASELINE=/path/to/smoke-results.json
export RSE_SMALL_FILE_BASELINE=/path/to/small-file-results.json
export RSE_RECONSTRUCTION_HOTSPOT_BASELINE=/path/to/reconstruction-hotspot-results.json
bash scripts/release-check.sh
```

### 3.3 触发链路（CI）

- Tag 推送触发 `release-preflight`
- `release-preflight` 成功后触发 `publish`
- 在仓库变量完整时预检是 `release` 模式；缺失任一基线变量时降级为 `extended`

### 3.4 Release Note 模板（标准）

```text
## Release Checklist
- Tag: v7.0.0
- Commit: <sha>
- Release-preflight mode: release
- Baselines:
  - Smoke: <path/artifact>
  - Small-file: <path/artifact>
  - Reconstruction hotspot: <path/artifact>
- Release-preflight: PASS
- Publish: PASS
- Publish time: <YYYY-MM-DD HH:MM:SS UTC>
```

### 3.5 Release Note 模板（变量缺失降级）

```text
## Release Checklist
- Tag: v7.0.0
- Commit: <sha>
- Release-preflight mode: extended (baseline incomplete)
- Baseline status:
  - Smoke: missing
  - Small-file: missing
  - Reconstruction hotspot: missing
- Release-preflight: PASS (degraded)
- Publish: PASS
- Publish time: <YYYY-MM-DD HH:MM:SS UTC>
- Notes: Baseline env not fully configured; initialize RSE_* vars in repo settings before next release candidate.
```

## 4. 变更摘要（发布候选复核）

- 核对 `scripts/release-check.sh` 已支持 `release` 严格模式。
- 对 7.0.0 changelog 条目进行正式化日期标注。
- 发布执行文档留存于本文件，便于标签发布与 PR 复核引用。

## 5. 最终可直接发布的 Release Note 示例（粘贴到 GitHub Release）

```text
## reed-solomon-erasure 7.0.0

### 发布信息
- Tag: v7.0.0
- Commit: <sha>
- 触发者: <GitHub 用户名>
- 发布时间: <YYYY-MM-DD HH:MM:SS UTC>

### 发布链路
- Tag 推送: <https://github.com/<owner>/<repo>/releases/tag/v7.0.0>
- release-preflight: [PASS](<release-preflight-action-url>)  (mode: release 或 extended)
- publish: [PASS](<publish-action-url>)

### baseline 配置
- RSE_SMOKE_BASELINE: <artifacts/benchmarks/7.0.0/smoke-results.json>
- RSE_SMALL_FILE_BASELINE: <artifacts/benchmarks/7.0.0/small-file-results.json>
- RSE_RECONSTRUCTION_HOTSPOT_BASELINE: <artifacts/benchmarks/7.0.0/reconstruction-hotspot-results.json>

### 发布自检
- CHANGELOG: 7.0.0 已从 Unreleased 固定为正式日期
- Version: 7.0.0
- 工作树: git status --short 为空
- release-check: 通过

### 证据
- Release Checklist: [记录链接或文本摘录](<release-note-checklist-url>)
- 基线对比报告:
  - Smoke: <path-or-url>
  - Small-file: <path-or-url>
  - Reconstruction hotspot: <path-or-url>
- 备注: <如果有例外、补充说明写在这里>
```

### 快速替换说明
- `<release-preflight-action-url>`：对应 CI 上 `release-preflight` 任务页面链接
- `<publish-action-url>`：对应 CI 上 `publish` 任务页面链接
- `<release-note-checklist-url>`：可放 release note 中 3.1~3.5 的记录位置
- `mode`：若三项基线变量完整，填 `release`；否则填 `extended` 并保持降级说明

### 5.1 可直接发布（请替换方括号内字段）

将以下内容保存为 `/tmp/release-7.0.0.md` 后执行 `gh release create`。

```text
## reed-solomon-erasure 7.0.0

### 发布信息
- Tag: v7.0.0
- Commit: [FULL_COMMIT_SHA]
- 触发者: [GH_USERNAME]
- 发布时间: [YYYY-MM-DD HH:MM:SS UTC]

### 发布链路
- Tag 推送: https://github.com/houseme/reed-solomon-erasure/releases/tag/v7.0.0
- release-preflight: [PASS](https://github.com/houseme/reed-solomon-erasure/actions/runs/[RELEASE_PREFLIGHT_RUN_ID])  (mode: [release|extended])
- publish: [PASS](https://github.com/houseme/reed-solomon-erasure/actions/runs/[PUBLISH_RUN_ID])

### baseline 配置
- RSE_SMOKE_BASELINE: [artifacts/benchmarks/7.0.0/smoke-results.json]
- RSE_SMALL_FILE_BASELINE: [artifacts/benchmarks/7.0.0/small-file-results.json]
- RSE_RECONSTRUCTION_HOTSPOT_BASELINE: [artifacts/benchmarks/7.0.0/reconstruction-hotspot-results.json]

### 发布自检
- CHANGELOG: 7.0.0 已从 Unreleased 固定为正式日期
- 版本: 7.0.0
- 工作树: git status --short 为空
- release-check: PASS

### 证据
- Release Checklist: [记录链接或文本摘录](https://github.com/houseme/reed-solomon-erasure/commit/[COMMIT_SHA]/checks)
- 小文件基线: [PATH_OR_URL]
- Hotspot 基线: [PATH_OR_URL]
- Smoke 基线: [PATH_OR_URL]
- 备注: [如有降级/例外，在此说明]
```

发布命令（将文件替换好后直接执行）：

```bash
gh release create v7.0.0 \
  --title "reed-solomon-erasure 7.0.0" \
  --notes-file /tmp/release-7.0.0.md \
  --target [BRANCH_OR_SHA]
```

建议把 `[RELEASE_PREFLIGHT_RUN_ID]`、`[PUBLISH_RUN_ID]` 先保存在 release note 里，便于审计追溯。  
