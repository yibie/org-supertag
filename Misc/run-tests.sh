#!/bin/bash

# 设置错误时退出
set -e

# 定义颜色输出
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 获取项目根目录的绝对路径
PROJECT_ROOT="/Users/chenyibin/Documents/emacs/package/org-supertag"
cd "$PROJECT_ROOT"

# 检查文件是否存在
check_file() {
    if [ ! -f "$PROJECT_ROOT/$1" ]; then
        echo -e "${RED}错误: 找不到文件 $PROJECT_ROOT/$1${NC}"
        exit 1
    else
        echo -e "${GREEN}找到文件: $PROJECT_ROOT/$1${NC}"
    fi
}

echo -e "${GREEN}开始运行 org-supertag 测试...${NC}"
echo -e "${YELLOW}项目根目录: $PROJECT_ROOT${NC}"

# 检查必要文件
echo -e "${YELLOW}检查必要文件...${NC}"
required_files=(
    "org-supertag-types.el"
    "org-supertag-api.el"
    "org-supertag-db.el"
    "org-supertag-cache.el"
    "org-supertag-parser.el"
    "org-supertag-group.el"
    "org-supertag-tag.el"
    "org-supertag-field.el"
    "org-supertag-node.el"
    "org-supertag-tests.el"
    "org-supertag-parser-test.el"
)

for file in "${required_files[@]}"; do
    check_file "$file"
done

echo -e "${YELLOW}加载依赖包...${NC}"

# 运行测试
emacs -Q --batch \
      --eval "(add-to-list 'load-path \"$PROJECT_ROOT\")" \
      --eval "(setq debug-on-error t)" \
      --eval "(require 'package)" \
      --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
      --eval "(package-initialize)" \
      --eval "(unless (package-installed-p 'ht) (package-refresh-contents) (package-install 'ht))" \
      --eval "(require 'org)" \
      --eval "(require 'ert)" \
      --eval "(message \"当前目录: %s\" default-directory)" \
      --eval "(message \"加载路径: %s\" load-path)" \
      --eval "(message \"开始加载测试文件...\")" \
      -l "$PROJECT_ROOT/org-supertag-types.el" \
      -l "$PROJECT_ROOT/org-supertag-api.el" \
      -l "$PROJECT_ROOT/org-supertag-db.el" \
      -l "$PROJECT_ROOT/org-supertag-cache.el" \
      -l "$PROJECT_ROOT/org-supertag-parser.el" \
      -l "$PROJECT_ROOT/org-supertag-group.el" \
      -l "$PROJECT_ROOT/org-supertag-tag.el" \
      -l "$PROJECT_ROOT/org-supertag-field.el" \
      -l "$PROJECT_ROOT/org-supertag-node.el" \
      -l "$PROJECT_ROOT/org-supertag-tests.el" \
      -l "$PROJECT_ROOT/org-supertag-parser-test.el" \
      --eval "(message \"所有文件加载完成\")" \
      -f ert-run-tests-batch-and-exit

# 获取测试结果
result=$?

# 输出结果
if [ $result -eq 0 ]; then
    echo -e "${GREEN}✓ 所有测试通过${NC}"
    exit 0
else
    echo -e "${RED}✗ 测试失败${NC}"
    exit 1
fi 